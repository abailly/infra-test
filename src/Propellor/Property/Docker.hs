{-# LANGUAGE BangPatterns #-}

-- | Docker support for propellor
--
-- The existance of a docker container is just another Property of a system,
-- which propellor can set up. See config.hs for an example.

module Propellor.Property.Docker (
	-- * Host properties
	installed,
	configured,
	container,
	docked,
	memoryLimited,
	garbageCollected,
	tweaked,
	containerStarted,
	Image,
	ContainerName,
	Container,
	-- * Container configuration
	dns,
	hostname,
	publish,
	expose,
	user,
	volume,
	volumes_from,
	workdir,
	memory,
	cpuShares,
	link,
	ContainerAlias,
	restartAlways,
	restartOnFailure,
	restartNever,
	-- * Internal use
	init,
	chain,
) where

import Propellor hiding (init)
import Propellor.Types.Docker
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Shim as Shim
import Utility.SafeCommand
import Utility.Path
import Utility.ThreadScheduler

import Control.Concurrent.Async hiding (link)
import System.Posix.Directory
import System.Posix.Process
import Prelude hiding (init)
import Data.List hiding (init)
import Data.List.Utils
import qualified Data.Map as M

installed :: Property
installed = Apt.installed ["docker"]

-- | Configures docker with an authentication file, so that images can be
-- pushed to index.docker.io. Optional.
configured :: Property
configured = prop `requires` installed
  where
	prop = withPrivData DockerAuthentication anyContext $ \getcfg ->
		property "docker configured" $ getcfg $ \cfg -> ensureProperty $ 
			"/root/.dockercfg" `File.hasContent` (lines cfg)

-- | A short descriptive name for a container.
-- Should not contain whitespace or other unusual characters,
-- only [a-zA-Z0-9_-] are allowed
type ContainerName = String

-- | A docker container.
data Container = Container Image Host

instance Hostlike Container where
	(Container i h) & p = Container i (h & p)
	(Container i h) &^ p = Container i (h &^ p)
	getHost (Container _ h) = h

-- | Defines a Container with a given name, image, and properties.
-- Properties can be added to configure the Container.
--
-- > container "web-server" "debian"
-- >    & publish "80:80"
-- >    & Apt.installed {"apache2"]
-- >    & ...
container :: ContainerName -> Image -> Container
container cn image = Container image (Host cn [] info)
  where
	info = dockerInfo mempty

-- | Ensures that a docker container is set up and running.
-- 
-- The container has its own Properties which are handled by running
-- propellor inside the container.
--
-- When the container's Properties include DNS info, such as a CNAME,
-- that is propigated to the Info of the Host it's docked in.
--
-- Reverting this property ensures that the container is stopped and
-- removed.
docked :: Container -> RevertableProperty
docked ctr@(Container _ h) = RevertableProperty
	(propigateContainerInfo ctr (go "docked" setup))
	(go "undocked" teardown)
  where
	cn = hostName h

	go desc a = property (desc ++ " " ++ cn) $ do
		hn <- asks hostName
		let cid = ContainerId hn cn
		ensureProperties [a cid (mkContainerInfo cid ctr)]

	setup cid (ContainerInfo image runparams) =
		provisionContainer cid
			`requires`
		runningContainer cid image runparams
			`requires`
		installed

	teardown cid (ContainerInfo image _runparams) =
		combineProperties ("undocked " ++ fromContainerId cid)
			[ stoppedContainer cid
			, property ("cleaned up " ++ fromContainerId cid) $
				liftIO $ report <$> mapM id
					[ removeContainer cid
					, removeImage image
					]
			]

propigateContainerInfo :: Container -> Property -> Property
propigateContainerInfo ctr@(Container _ h) p =
	propigateInfo ctr p (<> dockerinfo)
  where
	dockerinfo = dockerInfo $
		mempty { _dockerContainers = M.singleton (hostName h) h }

mkContainerInfo :: ContainerId -> Container -> ContainerInfo
mkContainerInfo cid@(ContainerId hn _cn) (Container img h) = 
	ContainerInfo img runparams
  where
	runparams = map (\(DockerRunParam mkparam) -> mkparam hn)
		(_dockerRunParams info)
	info = _dockerinfo $ hostInfo h'
	h' = h
		-- Restart by default so container comes up on
		-- boot or when docker is upgraded.
		&^ restartAlways
		-- Expose propellor directory inside the container.
		& volume (localdir++":"++localdir)
		-- Name the container in a predictable way so we
		-- and the user can easily find it later. This property
		-- comes last, so it cannot be overridden.
		& name (fromContainerId cid)

-- | Causes *any* docker images that are not in use by running containers to
-- be deleted. And deletes any containers that propellor has set up
-- before that are not currently running. Does not delete any containers
-- that were not set up using propellor.
--
-- Generally, should come after the properties for the desired containers.
garbageCollected :: Property
garbageCollected = propertyList "docker garbage collected"
	[ gccontainers
	, gcimages
	]
  where
	gccontainers = property "docker containers garbage collected" $
		liftIO $ report <$> (mapM removeContainer =<< listContainers AllContainers)
	gcimages = property "docker images garbage collected" $ do
		liftIO $ report <$> (mapM removeImage =<< listImages)

-- | Tweaks a container to work well with docker.
--
-- Currently, this consists of making pam_loginuid lines optional in
-- the pam config, to work around https://github.com/docker/docker/issues/5663
-- which affects docker 1.2.0.
tweaked :: Property
tweaked = trivial $
	cmdProperty "sh" ["-c", "sed -ri 's/^session\\s+required\\s+pam_loginuid.so$/session optional pam_loginuid.so/' /etc/pam.d/*"]
	`describe` "tweaked for docker"

-- | Configures the kernel to respect docker memory limits. 
--
-- This assumes the system boots using grub 2. And that you don't need any
-- other GRUB_CMDLINE_LINUX_DEFAULT settings.
--
-- Only takes effect after reboot. (Not automated.)
memoryLimited :: Property
memoryLimited = "/etc/default/grub" `File.containsLine` cfg
	`describe` "docker memory limited" 
	`onChange` cmdProperty "update-grub" []
  where
	cmdline = "cgroup_enable=memory swapaccount=1"
	cfg = "GRUB_CMDLINE_LINUX_DEFAULT=\""++cmdline++"\""

data ContainerInfo = ContainerInfo Image [RunParam]

-- | Parameters to pass to `docker run` when creating a container.
type RunParam = String

-- | A docker image, that can be used to run a container.
type Image = String

-- | Set custom dns server for container.
dns :: String -> Property
dns = runProp "dns"

-- | Set container host name.
hostname :: String -> Property
hostname = runProp "hostname"

-- | Set name of container.
name :: String -> Property
name = runProp "name"

-- | Publish a container's port to the host
-- (format: ip:hostPort:containerPort | ip::containerPort | hostPort:containerPort)
publish :: String -> Property
publish = runProp "publish"

-- | Expose a container's port without publishing it.
expose :: String -> Property
expose = runProp "expose"

-- | Username or UID for container.
user :: String -> Property
user = runProp "user"

-- | Mount a volume
-- Create a bind mount with: [host-dir]:[container-dir]:[rw|ro]
-- With just a directory, creates a volume in the container.
volume :: String -> Property
volume = runProp "volume"

-- | Mount a volume from the specified container into the current
-- container.
volumes_from :: ContainerName -> Property
volumes_from cn = genProp "volumes-from" $ \hn ->
	fromContainerId (ContainerId hn cn)

-- | Work dir inside the container. 
workdir :: String -> Property
workdir = runProp "workdir"

-- | Memory limit for container.
-- Format: <number><optional unit>, where unit = b, k, m or g
--
-- Note: Only takes effect when the host has the memoryLimited property
-- enabled.
memory :: String -> Property
memory = runProp "memory"

-- | CPU shares (relative weight).
--
-- By default, all containers run at the same priority, but you can tell
-- the kernel to give more CPU time to a container using this property.
cpuShares :: Int -> Property
cpuShares = runProp "cpu-shares" . show

-- | Link with another container on the same host.
link :: ContainerName -> ContainerAlias -> Property
link linkwith calias = genProp "link" $ \hn ->
	fromContainerId (ContainerId hn linkwith) ++ ":" ++ calias

-- | A short alias for a linked container.
-- Each container has its own alias namespace.
type ContainerAlias = String

-- | This property is enabled by default for docker containers configured by
-- propellor; as well as keeping badly behaved containers running,
-- it ensures that containers get started back up after reboot or
-- after docker is upgraded.
restartAlways :: Property
restartAlways = runProp "restart" "always"

-- | Docker will restart the container if it exits nonzero.
-- If a number is provided, it will be restarted only up to that many
-- times.
restartOnFailure :: Maybe Int -> Property
restartOnFailure Nothing = runProp "restart" "on-failure"
restartOnFailure (Just n) = runProp "restart" ("on-failure:" ++ show n)

-- | Makes docker not restart a container when it exits
-- Note that this includes not restarting it on boot!
restartNever :: Property
restartNever = runProp "restart" "no"

-- | A container is identified by its name, and the host
-- on which it's deployed.
data ContainerId = ContainerId 
	{ containerHostName :: HostName
	, containerName :: ContainerName
	}
	deriving (Eq, Read, Show)

-- | Two containers with the same ContainerIdent were started from
-- the same base image (possibly a different version though), and
-- with the same RunParams.
data ContainerIdent = ContainerIdent Image HostName ContainerName [RunParam]
	deriving (Read, Show, Eq)

toContainerId :: String -> Maybe ContainerId
toContainerId s
	| myContainerSuffix `isSuffixOf` s = case separate (== '.') (desuffix s) of
		(cn, hn)
			| null hn || null cn -> Nothing
			| otherwise -> Just $ ContainerId hn cn
	| otherwise = Nothing
  where
	desuffix = reverse . drop len . reverse
	len = length myContainerSuffix

fromContainerId :: ContainerId -> String
fromContainerId (ContainerId hn cn) = cn++"."++hn++myContainerSuffix

myContainerSuffix :: String
myContainerSuffix = ".propellor"

containerDesc :: ContainerId -> Property -> Property
containerDesc cid p = p `describe` desc
  where
	desc = "container " ++ fromContainerId cid ++ " " ++ propertyDesc p

runningContainer :: ContainerId -> Image -> [RunParam] -> Property
runningContainer cid@(ContainerId hn cn) image runps = containerDesc cid $ property "running" $ do
	l <- liftIO $ listContainers RunningContainers
	if cid `elem` l
		then checkident =<< liftIO getrunningident
		else ifM (liftIO $ elem cid <$> listContainers AllContainers)
			( do
				-- The container exists, but is not
				-- running. Its parameters may have
				-- changed, but we cannot tell without
				-- starting it up first.
				void $ liftIO $ startContainer cid
				-- It can take a while for the container to
				-- start up enough for its ident file to be
				-- written, so retry for up to 60 seconds.
				checkident =<< liftIO (retry 60 $ getrunningident)
			, go image
			)
  where
	ident = ContainerIdent image hn cn runps

	-- Check if the ident has changed; if so the
	-- parameters of the container differ and it must
	-- be restarted.
	checkident runningident
		| runningident == Just ident = noChange
		| otherwise = do
			void $ liftIO $ stopContainer cid
			restartcontainer

	restartcontainer = do
		oldimage <- liftIO $ fromMaybe image <$> commitContainer cid
		void $ liftIO $ removeContainer cid
		go oldimage

	getrunningident = readish
		<$> readProcess' (inContainerProcess cid [] ["cat", propellorIdent])

	retry :: Int -> IO (Maybe a) -> IO (Maybe a)
	retry 0 _ = return Nothing
	retry n a = do
		v <- a
		case v of
			Just _ -> return v
			Nothing -> do
				threadDelaySeconds (Seconds 1)				
				retry (n-1) a

	go img = do
		liftIO $ do
			clearProvisionedFlag cid
			createDirectoryIfMissing True (takeDirectory $ identFile cid)
		shim <- liftIO $ Shim.setup (localdir </> "propellor") Nothing (localdir </> shimdir cid)
		liftIO $ writeFile (identFile cid) (show ident)
		ensureProperty $ boolProperty "run" $ runContainer img
			(runps ++ ["-i", "-d", "-t"])
			[shim, "--continue", show (DockerInit (fromContainerId cid))]

-- | Called when propellor is running inside a docker container.
-- The string should be the container's ContainerId.
--
-- This process is effectively init inside the container.
-- It even needs to wait on zombie processes!
--
-- In the foreground, run an interactive bash (or sh) shell,
-- so that the user can interact with it when attached to the container.
--
-- When the system reboots, docker restarts the container, and this is run
-- again. So, to make the necessary services get started on boot, this needs
-- to provision the container then. However, if the container is already
-- being provisioned by the calling propellor, it would be redundant and
-- problimatic to also provisoon it here, when not booting up.
--
-- The solution is a flag file. If the flag file exists, then the container
-- was already provisioned. So, it must be a reboot, and time to provision
-- again. If the flag file doesn't exist, don't provision here.
init :: String -> IO ()
init s = case toContainerId s of
	Nothing -> error $ "Invalid ContainerId: " ++ s
	Just cid -> do
		changeWorkingDirectory localdir
		writeFile propellorIdent . show =<< readIdentFile cid
		whenM (checkProvisionedFlag cid) $ do
			let shim = Shim.file (localdir </> "propellor") (localdir </> shimdir cid)
			unlessM (boolSystem shim [Param "--continue", Param $ show $ toChain cid]) $
				warningMessage "Boot provision failed!"
		void $ async $ job reapzombies
		job $ do
			void $ tryIO $ ifM (inPath "bash")
				( boolSystem "bash" [Param "-l"]
				, boolSystem "/bin/sh" []
				)
			putStrLn "Container is still running. Press ^P^Q to detach."
  where
	job = forever . void . tryIO
	reapzombies = void $ getAnyProcessStatus True False

-- | Once a container is running, propellor can be run inside
-- it to provision it.
provisionContainer :: ContainerId -> Property
provisionContainer cid = containerDesc cid $ property "provisioned" $ liftIO $ do
	let shim = Shim.file (localdir </> "propellor") (localdir </> shimdir cid)
	let params = ["--continue", show $ toChain cid]
	msgh <- mkMessageHandle
	let p = inContainerProcess cid
		[ if isConsole msgh then "-it" else "-i" ]
		(shim : params)
	r <- withHandle StdoutHandle createProcessSuccess p $
		processChainOutput
	when (r /= FailedChange) $
		setProvisionedFlag cid 
	return r

toChain :: ContainerId -> CmdLine
toChain cid = DockerChain (containerHostName cid) (fromContainerId cid)

chain :: [Host] -> HostName -> String -> IO ()
chain hostlist hn s = case toContainerId s of
	Nothing -> errorMessage "bad container id"
	Just cid -> case findHostNoAlias hostlist hn of
		Nothing -> errorMessage ("cannot find host " ++ hn)
		Just parenthost -> case M.lookup (containerName cid) (_dockerContainers $ _dockerinfo $ hostInfo parenthost) of
			Nothing -> errorMessage ("cannot find container " ++ containerName cid ++ " docked on host " ++ hn)
			Just h -> go cid h
  where
	go cid h = do
		changeWorkingDirectory localdir
		onlyProcess (provisioningLock cid) $ do
			r <- runPropellor h $ ensureProperties $ hostProperties h
			putStrLn $ "\n" ++ show r

stopContainer :: ContainerId -> IO Bool
stopContainer cid = boolSystem dockercmd [Param "stop", Param $ fromContainerId cid ]

startContainer :: ContainerId -> IO Bool
startContainer cid = boolSystem dockercmd [Param "start", Param $ fromContainerId cid ]


-- | Simple property for starting a named container
-- This property is idempotent.
containerStarted :: ContainerName -> Property
containerStarted cname = property ("container " ++ cname ++ " is started") $
						 liftIO $ ifM (boolSystem dockercmd [Param "start", Param $ cname ])
						 ( return MadeChange , return FailedChange)

stoppedContainer :: ContainerId -> Property
stoppedContainer cid = containerDesc cid $ property desc $ 
	ifM (liftIO $ elem cid <$> listContainers RunningContainers)
		( liftIO cleanup `after` ensureProperty 
			(boolProperty desc $ stopContainer cid)
		, return NoChange
		)
  where
	desc = "stopped"
	cleanup = do
		nukeFile $ identFile cid
		removeDirectoryRecursive $ shimdir cid
		clearProvisionedFlag cid

removeContainer :: ContainerId -> IO Bool
removeContainer cid = catchBoolIO $
	snd <$> processTranscript dockercmd ["rm", fromContainerId cid ] Nothing

removeImage :: Image -> IO Bool
removeImage image = catchBoolIO $
	snd <$> processTranscript dockercmd ["rmi", image ] Nothing

runContainer :: Image -> [RunParam] -> [String] -> IO Bool
runContainer image ps cmd = boolSystem dockercmd $ map Param $
	"run" : (ps ++ image : cmd)

inContainerProcess :: ContainerId -> [String] -> [String] -> CreateProcess
inContainerProcess cid ps cmd = proc dockercmd ("exec" : ps ++ [fromContainerId cid] ++ cmd)

commitContainer :: ContainerId -> IO (Maybe Image)
commitContainer cid = catchMaybeIO $
	takeWhile (/= '\n') 
		<$> readProcess dockercmd ["commit", fromContainerId cid]

data ContainerFilter = RunningContainers | AllContainers
	deriving (Eq)

-- | Only lists propellor managed containers.
listContainers :: ContainerFilter -> IO [ContainerId]
listContainers status = 
	catMaybes . map toContainerId . concat . map (split ",")
		. catMaybes . map (lastMaybe . words) . lines
		<$> readProcess dockercmd ps
  where
	ps
		| status == AllContainers = baseps ++ ["--all"]
		| otherwise = baseps
	baseps = ["ps", "--no-trunc"]

listImages :: IO [Image]
listImages = lines <$> readProcess dockercmd ["images", "--all", "--quiet"]

runProp :: String -> RunParam -> Property
runProp field val = pureInfoProperty (param) $ dockerInfo $
	mempty { _dockerRunParams = [DockerRunParam (\_ -> "--"++param)] }
  where
	param = field++"="++val

genProp :: String -> (HostName -> RunParam) -> Property
genProp field mkval = pureInfoProperty field $ dockerInfo $
	mempty { _dockerRunParams = [DockerRunParam (\hn -> "--"++field++"=" ++ mkval hn)] }

dockerInfo :: DockerInfo Host -> Info
dockerInfo i = mempty { _dockerinfo = i }

-- | The ContainerIdent of a container is written to
-- /.propellor-ident inside it. This can be checked to see if
-- the container has the same ident later.
propellorIdent :: FilePath
propellorIdent = "/.propellor-ident"

provisionedFlag :: ContainerId -> FilePath
provisionedFlag cid = "docker" </> fromContainerId cid ++ ".provisioned"

clearProvisionedFlag :: ContainerId -> IO ()
clearProvisionedFlag = nukeFile . provisionedFlag

setProvisionedFlag :: ContainerId -> IO ()
setProvisionedFlag cid = do
	createDirectoryIfMissing True (takeDirectory (provisionedFlag cid))
	writeFile (provisionedFlag cid) "1"

checkProvisionedFlag :: ContainerId -> IO Bool
checkProvisionedFlag = doesFileExist . provisionedFlag

provisioningLock :: ContainerId -> FilePath
provisioningLock cid = "docker" </> fromContainerId cid ++ ".lock"

shimdir :: ContainerId -> FilePath
shimdir cid = "docker" </> fromContainerId cid ++ ".shim"

identFile :: ContainerId -> FilePath
identFile cid = "docker" </> fromContainerId cid ++ ".ident"

readIdentFile :: ContainerId -> IO ContainerIdent
readIdentFile cid = fromMaybe (error "bad ident in identFile")
	. readish <$> readFile (identFile cid)

dockercmd :: String
dockercmd = "docker"

report :: [Bool] -> Result
report rmed
	| or rmed = MadeChange
	| otherwise = NoChange

