-- | Support for running propellor, as built outside a container,
-- inside the container, without needing to install anything into the
-- container.
--
-- Note: This is currently Debian specific, due to glibcLibs.

module Propellor.Shim (setup, cleanEnv, file) where

import Propellor
import Utility.LinuxMkLibs
import Utility.SafeCommand
import Utility.FileMode
import Utility.FileSystemEncoding

import Data.List
import System.Posix.Files

-- | Sets up a shimmed version of the program, in a directory, and
-- returns its path.
--
-- Propellor may be running from an existing shim, in which case it's
-- simply reused.
setup :: FilePath -> Maybe FilePath -> FilePath -> IO FilePath
setup propellorbin propellorbinpath dest = checkAlreadyShimmed propellorbin $ do
	createDirectoryIfMissing True dest

	libs <- parseLdd <$> readProcess "ldd" [propellorbin]
	glibclibs <- glibcLibs
	let libs' = nub $ libs ++ glibclibs
	libdirs <- map (dest ++) . nub . catMaybes
		<$> mapM (installLib installFile dest) libs'
	
	let linker = (dest ++) $ 
		fromMaybe (error "cannot find ld-linux linker") $
			headMaybe $ filter ("ld-linux" `isInfixOf`) libs'
	let linkersym = takeDirectory linker </> takeFileName propellorbin
	createSymbolicLink linkersym (takeFileName linker)

	let gconvdir = (dest ++) $ takeDirectory $
		fromMaybe (error "cannot find gconv directory") $
			headMaybe $ filter ("/gconv/" `isInfixOf`) glibclibs
	let linkerparams = ["--library-path", intercalate ":" libdirs ]
	let shim = file propellorbin dest
	writeFile shim $ unlines
		[ shebang
		, "GCONV_PATH=" ++ shellEscape gconvdir
		, "export GCONV_PATH"
		, "exec " ++ unwords (map shellEscape $ linkersym : linkerparams) ++ 
			" " ++ shellEscape (fromMaybe propellorbin propellorbinpath) ++ " \"$@\""
		]
	modifyFileMode shim (addModes executeModes)
	return shim

shebang :: String
shebang = "#!/bin/sh"

checkAlreadyShimmed :: FilePath -> IO FilePath -> IO FilePath
checkAlreadyShimmed f nope = withFile f ReadMode $ \h -> do
	fileEncoding h
	s <- hGetLine h
	if s == shebang
		then return f
		else nope

-- Called when the shimmed propellor is running, so that commands it runs
-- don't see it.
cleanEnv :: IO ()
cleanEnv = void $ unsetEnv "GCONV_PATH"

file :: FilePath -> FilePath -> FilePath
file propellorbin dest = dest </> takeFileName propellorbin

installFile :: FilePath -> FilePath -> IO ()
installFile top f = do
	createDirectoryIfMissing True destdir
	nukeFile dest
	createLink f dest `catchIO` (const copy)
  where
	copy = void $ boolSystem "cp" [Param "-a", Param f, Param dest]
	destdir = inTop top $ takeDirectory f
	dest = inTop top f
