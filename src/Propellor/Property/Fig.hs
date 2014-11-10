{-# LANGUAGE BangPatterns #-}

-- | fig support for propellor
--
-- Fig is a tool to manage docker containers, starting all containers in order
-- according to a `fig.yml` configuration file.
module Propellor.Property.Fig  where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

import Utility.FileMode
import System.Posix.Files

installed :: Property
installed = prop `requires` Apt.installed ["docker.io"]
  where
	prop = combineProperties "fig installed"
			   [ cmdProperty "installing fig"
							 [ "curl -L https://github.com/docker/fig/releases/download/1.0.1/fig-`uname -s`-`uname -m` > /usr/local/bin/fig" ]
						   , File.mode "/usr/local/bin/fig" (combineModes  (ownerWriteMode:readModes ++ executeModes))
						   ]
