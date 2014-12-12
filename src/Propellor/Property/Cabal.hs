-- | Module for interacting with cabal installer
module Propellor.Property.Cabal where

import Data.List
import Propellor
import Propellor.Property.User

import Utility.SafeCommand

type PackageName = String

updated :: UserName -> Property
updated user = property ("update cabal and all packages for user " ++ user) $ do
                  home <- liftIO $ homedir user
                  ensureProperty $ userScriptProperty user [ "cabal update"
														   , "cabal install cabal-install"
														   , "echo 'export PATH=$PATH:" ++ home ++ "/.cabal/bin' >> " ++ home ++ "/.bash_profile"
														   ]
		  
-- |Install latest versions of the listed packages
installed :: UserName -> [ PackageName ] -> Property
installed user pkgs = prop
  where
	pkgList =  concat (intersperse " " (map shellEscape pkgs))
	prop = property ("install packages " ++ pkgList) $ ensureProperty $ 
		   userScriptProperty user [ "cabal install " ++ pkgList  ]


