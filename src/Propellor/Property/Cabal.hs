-- | Module for interacting with cabal installer
module Propellor.Property.Cabal where

import           Data.List
import           Propellor
import           Propellor.Property.File as File
import           Propellor.Property.User

import           Data.Monoid             ((<>))
import           Utility.SafeCommand

type PackageName = String

updated :: UserName -> Property NoInfo
updated user = property ("update cabal and all packages for user " ++ user) $ do
                  home <- liftIO $ homedir user
                  ensureProperty $ userScriptProperty user [ "cabal update" ]
					`before`
					File.containsLine (home </> ".bash_profile") ( "export PATH=" ++ home ++ "/.cabal/bin:$PATH")



-- |Install latest versions of the listed packages
installed :: UserName -> [ PackageName ] -> Property NoInfo
installed user pkgs = prop
  where
	pkgList =  concat (intersperse " " (map shellEscape pkgs))
	prop = property ("install packages " ++ pkgList) $ ensureProperty $
		   userScriptProperty user [ "cabal install " ++ pkgList  ]

toolsInstalledInSandbox :: UserName -> FilePath -> [ PackageName] -> Property NoInfo
toolsInstalledInSandbox user path pkgs = prop
  where
	pkgList =  concat (intersperse " " (map shellEscape pkgs))
	prop = property ("install packages " ++ pkgList) $ ensureProperty $
		   userScriptProperty user ["mkdir -p " <> path,
                                            "cd " <> path,
                                            "cabal sandbox init", -- might fail on second run
                                            "cabal install " ++ pkgList  ]
