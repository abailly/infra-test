-- | Module for interacting with cabal installer
module Propellor.Property.Cabal where

import           Data.List
import           Propellor
import           Propellor.Property.File as File
import           Propellor.Property.User

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


-- sandBoxed :: UserName -> FilePath -> Propert NoInfo


toolsInstalledInSandbox :: UserName -> FilePath -> [ PackageName] -> Property NoInfo
toolsInstalledInSandbox user path pkgs = propertyList "Install packages in sandbox" [packageProp]
  where
	pkgList =  concat (intersperse " " (map shellEscape pkgs))
	packageProp = userScriptProperty user ["mkdir -p " <> path,
                                               "cd " <> path,
                                               cabal <> "sandbox init", -- just continues if there is already a sandbox
                                               cabal <> "install " ++ pkgList  ]

        cabal = "/opt/cabal/1.20/bin/cabal --with-compiler=/opt/ghc/7.8.3/bin/ghc " -- make it easy to use custom cabal
