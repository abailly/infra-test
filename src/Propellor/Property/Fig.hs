{-# LANGUAGE BangPatterns #-}

-- | fig support for propellor
--
-- Fig is a tool to manage docker containers, starting all containers in order
-- according to a `fig.yml` configuration file.
module Propellor.Property.Fig  where

import Propellor

import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.File as File

import Utility.FileMode
import System.Posix.Files

-- | Installs fig as an executable in `/usr/local/bin/fig`
--
-- This is a very crude, unsecure and gross way of doing it, but it works fine as long
-- as the `uname` is available and corresponds to exsting distributions for fig.
installed :: Property
installed = prop `requires` Docker.installed
  where
	prop = check (not <$> doesFileExist "/usr/local/bin/fig") $
		   combineProperties "fig installed"
			   [ scriptProperty [ "curl -L https://github.com/docker/fig/releases/download/1.0.1/fig-`uname -s`-`uname -m` > /usr/local/bin/fig" ]
			   , File.mode "/usr/local/bin/fig" (combineModes  (ownerWriteMode:readModes ++ executeModes))
			   ]
