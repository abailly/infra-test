module Propellor.Spin (
	commitSpin,
	spin,
	update,
	gitPushHelper,
	mergeSpin,
) where

import Data.List
import System.Exit
import System.PosixCompat
import System.Posix.IO
import System.Posix.Directory
import Control.Concurrent.Async
import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Network.BSD as BSD
import Network.Socket (inet_ntoa)

import Propellor
import Propellor.Protocol
import Propellor.PrivData.Paths
import Propellor.Git
import Propellor.Ssh
import Propellor.Gpg
import Propellor.Bootstrap
import Propellor.Types.CmdLine
import qualified Propellor.Shim as Shim
import Utility.FileMode
import Utility.SafeCommand

commitSpin :: IO ()
commitSpin = do
	void $ actionMessage "Git commit" $
		gitCommit [Param "--allow-empty", Param "-a", Param "-m", Param spinCommitMessage]
	-- Push to central origin repo first, if possible.
	-- The remote propellor will pull from there, which avoids
	-- us needing to send stuff directly to the remote host.
	whenM hasOrigin $
		void $ actionMessage "Push to central git repository" $
			boolSystem "git" [Param "push"]

spin :: HostName -> Maybe HostName -> Host -> IO ()
spin target relay hst = do
	cacheparams <- if viarelay
		then pure ["-A"]
		else toCommand <$> sshCachingParams hn
	when viarelay $
		void $ boolSystem "ssh-add" []

	sshtarget <- ("root@" ++) <$> case relay of
		Just r -> pure r
		Nothing -> getSshTarget target hst

	-- Install, or update the remote propellor.
	updateServer target relay hst
		(proc "ssh" $ cacheparams ++ [sshtarget, shellWrap probecmd])
		(proc "ssh" $ cacheparams ++ [sshtarget, shellWrap updatecmd])

	-- And now we can run it.
	unlessM (boolSystem "ssh" (map Param $ cacheparams ++ ["-t", sshtarget, shellWrap runcmd])) $
		error $ "remote propellor failed"
  where
	hn = fromMaybe target relay

	relaying = relay == Just target
	viarelay = isJust relay && not relaying

	probecmd = intercalate " ; "
		[ "if [ ! -d " ++ localdir ++ "/.git ]"
		, "then (" ++ intercalate " && "
			[ installGitCommand
			, "echo " ++ toMarked statusMarker (show NeedGitClone)
			] ++ ") || echo " ++ toMarked statusMarker (show NeedPrecompiled)
		, "else " ++ updatecmd
		, "fi"
		]
	
	updatecmd = intercalate " && "
		[ "cd " ++ localdir
		, bootstrapPropellorCommand
		, if viarelay
			then "./propellor --continue " ++
				shellEscape (show (Relay target))
			-- Still using --boot for back-compat...
			else "./propellor --boot " ++ target
		]

	runcmd = "cd " ++ localdir ++ " && ./propellor " ++ cmd
	cmd = if viarelay
		then "--serialized " ++ shellEscape (show (Spin [target] (Just target)))
		else "--continue " ++ shellEscape (show (SimpleRun target))

-- Check if the Host contains an IP address that matches one of the IPs
-- in the DNS for the HostName. If so, the HostName is used as-is, 
-- but if the DNS is out of sync with the Host config, or doesn't have
-- the host in it at all, use one of the Host's IPs instead.
getSshTarget :: HostName -> Host -> IO String
getSshTarget target hst
	| null configips = return target
	| otherwise = go =<< tryIO (BSD.getHostByName target)
  where
	go (Left e) = useip (show e)
	go (Right hostentry) = ifM (anyM matchingconfig (BSD.hostAddresses hostentry))
		( return target
		, do
			ips <- mapM inet_ntoa (BSD.hostAddresses hostentry)
			useip ("DNS " ++ show ips ++ " vs configured " ++ show configips)
		)

	matchingconfig a = flip elem configips <$> inet_ntoa a

	useip why = case headMaybe configips of
		Nothing -> return target
		Just ip -> do
			-- If we're being asked to run on the local host,
			-- ignore DNS.
			s <- takeWhile (/= '\n') <$> readProcess "hostname" ["-f"]
			if s == target
				then return target
				else do
					warningMessage $ "DNS seems out of date for " ++ target ++ " (" ++ why ++ "); using IP address from configuration instead."
					return ip

	configips = map fromIPAddr $ mapMaybe getIPAddr $
		S.toList $ _dns $ hostInfo hst

-- Update the privdata, repo url, and git repo over the ssh
-- connection, talking to the user's local propellor instance which is
-- running the updateServer
update :: Maybe HostName -> IO ()
update forhost = do
	whenM hasGitRepo $
		req NeedRepoUrl repoUrlMarker setRepoUrl

	makePrivDataDir
	createDirectoryIfMissing True (takeDirectory privfile)
	req NeedPrivData privDataMarker $
		writeFileProtected privfile

	whenM hasGitRepo $
		req NeedGitPush gitPushMarker $ \_ -> do
			hin <- dup stdInput
			hout <- dup stdOutput
			hClose stdin
			hClose stdout
			unlessM (boolSystem "git" (pullparams hin hout)) $
				errorMessage "git pull from client failed"
  where
	pullparams hin hout =
		[ Param "pull"
		, Param "--progress"
		, Param "--upload-pack"
		, Param $ "./propellor --gitpush " ++ show hin ++ " " ++ show hout
		, Param "."
		]
	
	-- When --spin --relay is run, get a privdata file
	-- to be relayed to the target host.
	privfile = maybe privDataLocal privDataRelay forhost

updateServer
	:: HostName
	-> Maybe HostName
	-> Host
	-> CreateProcess
	-> CreateProcess
	-> IO ()
updateServer target relay hst connect haveprecompiled =
	withBothHandles createProcessSuccess connect go
  where
	hn = fromMaybe target relay
	relaying = relay == Just target

	go (toh, fromh) = do
		let loop = go (toh, fromh)
		let restart = updateServer hn relay hst connect haveprecompiled
		let done = return ()
		v <- (maybe Nothing readish <$> getMarked fromh statusMarker)
		case v of
			(Just NeedRepoUrl) -> do
				sendRepoUrl toh
				loop
			(Just NeedPrivData) -> do
				sendPrivData hn hst toh relaying
				loop
			(Just NeedGitClone) -> do
				hClose toh
				hClose fromh
				sendGitClone hn
				restart
			(Just NeedPrecompiled) -> do
				hClose toh
				hClose fromh
				sendPrecompiled hn
				updateServer hn relay hst haveprecompiled (error "loop")
			(Just NeedGitPush) -> do
				sendGitUpdate hn fromh toh
				hClose fromh
				hClose toh
				done
			Nothing -> done

sendRepoUrl :: Handle -> IO ()
sendRepoUrl toh = sendMarked toh repoUrlMarker =<< (fromMaybe "" <$> getRepoUrl)

sendPrivData :: HostName -> Host -> Handle -> Bool -> IO ()
sendPrivData hn hst toh relaying = do
	privdata <- getdata
	void $ actionMessage ("Sending privdata (" ++ show (length privdata) ++ " bytes) to " ++ hn) $ do
		sendMarked toh privDataMarker privdata
		return True
  where
	getdata
		| relaying = do
			let f = privDataRelay hn
			d <- readFileStrictAnyEncoding f
			nukeFile f
			return d
		| otherwise = show . filterPrivData hst <$> decryptPrivData

sendGitUpdate :: HostName -> Handle -> Handle -> IO ()
sendGitUpdate hn fromh toh =
	void $ actionMessage ("Sending git update to " ++ hn) $ do
		sendMarked toh gitPushMarker ""
		(Nothing, Nothing, Nothing, h) <- createProcess p
		(==) ExitSuccess <$> waitForProcess h
  where
	p = (proc "git" ["upload-pack", "."])
		{ std_in = UseHandle fromh
		, std_out = UseHandle toh
		}

-- Initial git clone, used for bootstrapping.
sendGitClone :: HostName -> IO ()
sendGitClone hn = void $ actionMessage ("Clone git repository to " ++ hn) $ do
	branch <- getCurrentBranch
	cacheparams <- sshCachingParams hn
	withTmpFile "propellor.git" $ \tmp _ -> allM id
		[ boolSystem "git" [Param "bundle", Param "create", File tmp, Param "HEAD"]
		, boolSystem "scp" $ cacheparams ++ [File tmp, Param ("root@"++hn++":"++remotebundle)]
		, boolSystem "ssh" $ cacheparams ++ [Param ("root@"++hn), Param $ unpackcmd branch]
		]
  where
	remotebundle = "/usr/local/propellor.git"
	unpackcmd branch = shellWrap $ intercalate " && "
		[ "git clone " ++ remotebundle ++ " " ++ localdir
		, "cd " ++ localdir
		, "git checkout -b " ++ branch
		, "git remote rm origin"
		, "rm -f " ++ remotebundle
		]

-- Send a tarball containing the precompiled propellor, and libraries.
-- This should be reasonably portable, as long as the remote host has the
-- same architecture as the build host.
sendPrecompiled :: HostName -> IO ()
sendPrecompiled hn = void $ actionMessage ("Uploading locally compiled propellor as a last resort") $ do
	bracket getWorkingDirectory changeWorkingDirectory $ \_ ->
		withTmpDir "propellor" go
  where
	go tmpdir = do
		cacheparams <- sshCachingParams hn
		let shimdir = takeFileName localdir
		createDirectoryIfMissing True (tmpdir </> shimdir)
		changeWorkingDirectory (tmpdir </> shimdir)
		me <- readSymbolicLink "/proc/self/exe"
		createDirectoryIfMissing True "bin"
		unlessM (boolSystem "cp" [File me, File "bin/propellor"]) $
			errorMessage "failed copying in propellor"
		let bin = "bin/propellor"
		let binpath = Just $ localdir </> bin
		void $ Shim.setup bin binpath "."
		changeWorkingDirectory tmpdir
		withTmpFile "propellor.tar." $ \tarball _ -> allM id
			[ boolSystem "strip" [File me]
			, boolSystem "tar" [Param "czf", File tarball, File shimdir]
			, boolSystem "scp" $ cacheparams ++ [File tarball, Param ("root@"++hn++":"++remotetarball)]
			, boolSystem "ssh" $ cacheparams ++ [Param ("root@"++hn), Param unpackcmd]
			]

	remotetarball = "/usr/local/propellor.tar"

	unpackcmd = shellWrap $ intercalate " && "
		[ "cd " ++ takeDirectory remotetarball
		, "tar xzf " ++ remotetarball
		, "rm -f " ++ remotetarball
		]

-- Shim for git push over the propellor ssh channel.
-- Reads from stdin and sends it to hout;
-- reads from hin and sends it to stdout.
gitPushHelper :: Fd -> Fd -> IO ()
gitPushHelper hin hout = void $ fromstdin `concurrently` tostdout
  where
	fromstdin = do
		h <- fdToHandle hout
		connect stdin h
	tostdout = do
		h <- fdToHandle hin
		connect h stdout
	connect fromh toh = do
		hSetBinaryMode fromh True
		hSetBinaryMode toh True
		b <- B.hGetSome fromh 40960
		if B.null b
			then do
				hClose fromh
				hClose toh
			else do
				B.hPut toh b
				hFlush toh
				connect fromh toh

mergeSpin :: IO ()
mergeSpin = do
	branch <- getCurrentBranch
	branchref <- getCurrentBranchRef
	old_head <- getCurrentGitSha1 branch
	old_commit <- findLastNonSpinCommit
	rungit "reset" [Param old_commit]
	rungit "commit" [Param "-a", Param "--allow-empty"]
	rungit "merge" =<< gpgSignParams [Param "-s", Param "ours", Param old_head]
	current_commit <- getCurrentGitSha1 branch
	rungit "update-ref" [Param branchref, Param current_commit]
	rungit "checkout" [Param branch]
  where
	rungit cmd ps = unlessM (boolSystem "git" (Param cmd:ps)) $
		error ("git " ++ cmd ++ " failed")

findLastNonSpinCommit :: IO String
findLastNonSpinCommit = do
	commits <- map (separate (== ' ')) . lines
		<$> readProcess "git" ["log", "--oneline", "--no-abbrev-commit"]
	case dropWhile (\(_, msg) -> msg == spinCommitMessage) commits of
		((sha, _):_) -> return sha
		_ -> error $ "Did not find any previous commit that was not a " ++ show spinCommitMessage

spinCommitMessage :: String
spinCommitMessage = "propellor spin"
