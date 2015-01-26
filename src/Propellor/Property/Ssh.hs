module Propellor.Property.Ssh (
	PubKeyText,
	setSshdConfig,
	permitRootLogin,
	passwordAuthentication,
	hasAuthorizedKeys,
	authorizedKey,
	restarted,
	randomHostKeys,
	hostKeys,
	hostKey,
	pubKey,
	getPubKey,
	keyImported,
	knownHost,
	authorizedKeys,
	listenPort,
	knownExternalHost
) where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.User
import Utility.SafeCommand
import Utility.FileMode

import System.PosixCompat
import qualified Data.Map as M

type PubKeyText = String

sshBool :: Bool -> String
sshBool True = "yes"
sshBool False = "no"

sshdConfig :: FilePath
sshdConfig = "/etc/ssh/sshd_config"

setSshdConfig :: String -> Bool -> Property NoInfo
setSshdConfig setting allowed = combineProperties "sshd config"
	[ sshdConfig `File.lacksLine` (sshline $ not allowed)
	, sshdConfig `File.containsLine` (sshline allowed)
	]
	`onChange` restarted
	`describe` unwords [ "ssh config:", setting, sshBool allowed ]
  where
	sshline v = setting ++ " " ++ sshBool v

permitRootLogin :: Bool -> Property NoInfo
permitRootLogin = setSshdConfig "PermitRootLogin"

passwordAuthentication :: Bool -> Property NoInfo
passwordAuthentication = setSshdConfig "PasswordAuthentication"

dotDir :: UserName -> IO FilePath
dotDir user = do
	h <- homedir user
	return $ h </> ".ssh"

dotFile :: FilePath -> UserName -> IO FilePath
dotFile f user = do
	d <- dotDir user
	return $ d </> f

hasAuthorizedKeys :: UserName -> IO Bool
hasAuthorizedKeys = go <=< dotFile "authorized_keys"
  where
	go f = not . null <$> catchDefaultIO "" (readFile f)

restarted :: Property NoInfo
restarted = Service.restarted "ssh"

-- | Blows away existing host keys and make new ones.
-- Useful for systems installed from an image that might reuse host keys.
-- A flag file is used to only ever do this once.
randomHostKeys :: Property NoInfo
randomHostKeys = flagFile prop "/etc/ssh/.unique_host_keys"
	`onChange` restarted
  where
	prop = property "ssh random host keys" $ do
		void $ liftIO $ boolSystem "sh"
			[ Param "-c"
			, Param "rm -f /etc/ssh/ssh_host_*"
			]
		ensureProperty $ scriptProperty 
			[ "DPKG_MAINTSCRIPT_NAME=postinst DPKG_MAINTSCRIPT_PACKAGE=openssh-server /var/lib/dpkg/info/openssh-server.postinst configure" ]

-- | Installs the specified list of ssh host keys.
--
-- The corresponding private keys come from the privdata.
--
-- Any host keysthat are not in the list are removed from the host.
hostKeys :: IsContext c => c -> [(SshKeyType, PubKeyText)] -> Property HasInfo
hostKeys ctx l = propertyList desc $ catMaybes $
	map (\(t, pub) -> Just $ hostKey ctx t pub) l ++ [cleanup]
  where
	desc = "ssh host keys configured " ++ typelist (map fst l)
	typelist tl = "(" ++ unwords (map fromKeyType tl) ++ ")"
	alltypes = [minBound..maxBound]
	staletypes = let have = map fst l in filter (`notElem` have) alltypes
	removestale b = map (File.notPresent . flip keyFile b) staletypes
	cleanup
		| null staletypes || null l = Nothing
		| otherwise = Just $ toProp $
			property ("any other ssh host keys removed " ++ typelist staletypes) $
				ensureProperty $
					combineProperties desc (removestale True ++ removestale False)
					`onChange` restarted

-- | Installs a single ssh host key of a particular type.
--
-- The public key is provided to this function;
-- the private key comes from the privdata; 
hostKey :: IsContext c => c -> SshKeyType -> PubKeyText -> Property HasInfo
hostKey context keytype pub = combineProperties desc
	[ pubKey keytype pub
	, toProp $ property desc $ install writeFile True pub
	, withPrivData (keysrc "" (SshPrivKey keytype "")) context $ \getkey ->
		property desc $ getkey $ install writeFileProtected False
	]
	`onChange` restarted
  where
	desc = "ssh host key configured (" ++ fromKeyType keytype ++ ")"
	install writer ispub key = do
		let f = keyFile keytype ispub
		s <- liftIO $ catchDefaultIO "" $ readFileStrict f
		if s == key
			then noChange
			else makeChange $ writer f key
	keysrc ext field = PrivDataSourceFileFromCommand field ("sshkey"++ext)
		("ssh-keygen -t " ++ sshKeyTypeParam keytype ++ " -f sshkey")

keyFile :: SshKeyType -> Bool -> FilePath
keyFile keytype ispub = "/etc/ssh/ssh_host_" ++ fromKeyType keytype ++ "_key" ++ ext
  where
	ext = if ispub then ".pub" else ""

-- | Indicates the host key that is used by a Host, but does not actually
-- configure the host to use it. Normally this does not need to be used;
-- use 'hostKey' instead.
pubKey :: SshKeyType -> PubKeyText -> Property HasInfo
pubKey t k = pureInfoProperty ("ssh pubkey known") $
	mempty { _sshPubKey = M.singleton t k }

getPubKey :: Propellor (M.Map SshKeyType String)
getPubKey = asks (_sshPubKey . hostInfo)

-- | Sets up a user with a ssh private key and public key pair from the
-- PrivData.
keyImported :: IsContext c => SshKeyType -> UserName -> c -> Property HasInfo
keyImported keytype user context = combineProperties desc
	[ installkey (SshPubKey keytype user) (install writeFile ".pub")
	, installkey (SshPrivKey keytype user) (install writeFileProtected "")
	]
  where
	desc = user ++ " has ssh key (" ++ fromKeyType keytype ++ ")"
	installkey p a = withPrivData p context $ \getkey ->
		property desc $ getkey a
	install writer ext key = do
		f <- liftIO $ keyfile ext
		ifM (liftIO $ doesFileExist f)
			( noChange
			, ensureProperties
				[ property desc $ makeChange $ do
					createDirectoryIfMissing True (takeDirectory f)
					writer f key
				, File.ownerGroup f user user
				, File.ownerGroup (takeDirectory f) user user
				]
			)
	keyfile ext = do
		home <- homeDirectory <$> getUserEntryForName user
		return $ home </> ".ssh" </> "id_" ++ fromKeyType keytype ++ ext

fromKeyType :: SshKeyType -> String
fromKeyType SshRsa = "rsa"
fromKeyType SshDsa = "dsa"
fromKeyType SshEcdsa = "ecdsa"
fromKeyType SshEd25519 = "ed25519"

-- | Puts some host's ssh public key(s), as set using 'pubKey',
-- into the known_hosts file for a user.
knownHost :: [Host] -> HostName -> UserName -> Property NoInfo
knownHost hosts hn user = property desc $
	go =<< fromHost hosts hn getPubKey
  where
	desc = user ++ " knows ssh key for " ++ hn
	go (Just m) | not (M.null m) = do
		f <- liftIO $ dotFile "known_hosts" user
		ensureProperty $ combineProperties desc
			[ File.dirExists (takeDirectory f)
			, f `File.containsLines`
				(map (\k -> hn ++ " " ++ k) (M.elems m))
			, File.ownerGroup f user user
			]
	go _ = do
		warningMessage $ "no configred pubKey for " ++ hn
		return FailedChange

-- | Adds some external host's public key to the known_hosts file for a user
-- This requires ssh to be installed and the 'ssh-keyscan' program to run
knownExternalHost :: HostName -> UserName -> Property NoInfo
knownExternalHost hn user = property desc go
  where
	desc = ("add key for " ++ hn ++ " to known_hosts for " ++ user)
	go = do
		f <- liftIO $ dotFile "known_hosts" user
		ensureProperty $ combineProperties desc
			[ File.dirExists (takeDirectory f)
			, userScriptProperty user $ [ "/usr/bin/ssh-keyscan " ++ shellEscape hn ++ " > " ++ f ]
			, File.ownerGroup f user user
			]

							
-- | Makes a user have authorized_keys from the PrivData
--
-- This removes any other lines from the file.
authorizedKeys :: IsContext c => UserName -> c -> Property HasInfo
authorizedKeys user context = withPrivData (SshAuthorizedKeys user) context $ \get ->
	property (user ++ " has authorized_keys") $ get $ \v -> do
		f <- liftIO $ dotFile "authorized_keys" user
		liftIO $ do
			createDirectoryIfMissing True (takeDirectory f)
			writeFileProtected f v
		ensureProperties 
			[ File.ownerGroup f user user
			, File.ownerGroup (takeDirectory f) user user
			] 

-- | Ensures that a user's authorized_keys contains a line.
-- Any other lines in the file are preserved as-is.
authorizedKey :: UserName -> String -> Property NoInfo
authorizedKey user l = property (user ++ " has autorized_keys line " ++ l) $ do
	f <- liftIO $ dotFile "authorized_keys" user
	ensureProperty $
		f `File.containsLine` l
			`requires` File.dirExists (takeDirectory f)
			`onChange` File.mode f (combineModes [ownerWriteMode, ownerReadMode])

-- | Makes the ssh server listen on a given port, in addition to any other
-- ports it is configured to listen on.
--
-- Revert to prevent it listening on a particular port.
listenPort :: Int -> RevertableProperty
listenPort port = enable <!> disable
  where
	portline = "Port " ++ show port
	enable = sshdConfig `File.containsLine` portline
		`describe` ("ssh listening on " ++ portline)
		`onChange` restarted
	disable = sshdConfig `File.lacksLine` portline
		`describe` ("ssh not listening on " ++ portline)
		`onChange` restarted
