-- | Module for interacting with cabal installer
module Propellor.Property.Cabal where

import Data.List
import Propellor

import Utility.SafeCommand

type PackageName = String

updated :: UserName -> Property
updated user = property ("update cabal and all packages for user " ++ user) $
			   ensureProperty $
			   userScriptProperty user [ "cabal update" , "cabal install cabal-install"  ]
		  
-- |Install latest versions of the listed packages
installed :: UserName -> [ PackageName ] -> Property
installed user pkgs = prop `requires` updated user
  where
	pkgList =  concat (intersperse " " (map shellEscape pkgs))
	prop = property ("install packages " ++ pkgList) $ ensureProperty $ 
		   userScriptProperty user [ "cabal install" ++ pkgList  ]


