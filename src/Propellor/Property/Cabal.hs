-- | Module for interacting with cabal installer
module Propellor.Property.Cabal where

import Data.List
import Propellor

type PackageName = String

updated :: Property
updated = propertyList "updated cabal and all packages"
		  [ cmdProperty "cabal" [ "update" ]
	      , cmdProperty "cabal" [ "install" , "cabal-install" ]
	      ]
		  
-- |Install latest versions of the listed packages
installed :: [ PackageName ] -> Property
installed pkgs = prop `requires` updated
  where
	prop = cmdProperty "cabal" ("install": intersperse " " pkgs)


