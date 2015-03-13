{-# LANGUAGE DoAndIfThenElse #-}

-- | To install latest docker from docker.com
-- Ubuntu is quite far behind, so we don't want to use Propellors' default docker support which will
-- replace a recent docker with ubuntu's one.
module Capital.Property.Docker (installLatestDocker
                               ,dockerEnabledFor
                               ,dockerAuthTokenFor
                               ,hasDataContainer
                               ,PropDocker.ContainerName,
                                PropDocker.Image,
                                createImage) where

import           Propellor
import qualified Propellor.Property.Apt    as Apt
import qualified Propellor.Property.Docker as PropDocker
import qualified Propellor.Property.File   as File
import qualified Propellor.Property.Sudo   as Sudo

import           Data.List                 as L

import qualified Utility.SafeCommand       as C

dockerEnabledFor :: String -> Property NoInfo
dockerEnabledFor =  Sudo.binaryEnabledFor "/usr/bin/docker"

-- configure docker authent to pull images from dockerhub
dockerAuthTokenFor :: String -> Property HasInfo
dockerAuthTokenFor _ =
  withPrivData (PrivFile "docker-auth-token") (Context "dev")
	(\ getdata -> property "docker auth configured"
	  		$ getdata $ \ tok -> liftIO $ (writeFile "/home/build/.dockercfg" (unlines
        [ "{"
	, "\"https://index.docker.io/v1/\":"
	, "   {\"auth\":\""  ++ tok ++ "\""
	, ", \"email\":\"dev@capital-match.com\"}"
	, "}"
	]) >> return MadeChange) `catchIO` const (return FailedChange))

installLatestDocker :: Property NoInfo
installLatestDocker = propertyList "install latest docker from official repositories"
                      [
                        cmdProperty "apt-key" [ "adv"
                                              , "--keyserver"
                                              , "hkp://keyserver.ubuntu.com:80"
                                              , "--recv-keys"
                                              , "36A1D7869245C8950F966E92D8576A8BA88D21E9"
                                              ]
                      , File.containsLines "/etc/apt/sources.list.d/docker.list"
                        ["deb https://get.docker.com/ubuntu docker main"]
                      , Apt.update
                      , Apt.installed [ "lxc-docker" ]
                      ]

-- |Create an image for a data volume container
-- We use an image, so that we can create a user and group inside of it, and ensure the permissions for the data volume inside the container match
-- TODO make user and volume configurable
createImage :: PropDocker.Image -> FilePath -> Property NoInfo
createImage image from = property "docker creates image from dockerfile" $ liftIO $ do
  toResult <$> C.boolSystem "docker" (map C.Params ["build","-t ",image," ",from])

-- |Create a data container only if it does not yet exist on the host
-- base image configurable because we might want to change users and what path is in the container
-- might change it to a list of paths later.
-- example: hasDataContainer "cm-data" "capital/app"
hasDataContainer :: PropDocker.ContainerName -> PropDocker.Image -> Property NoInfo
hasDataContainer name image = property "docker creates data-only container" $ liftIO $ do
  (containers,res) <- processTranscript "docker" ["ps", "-a"] Nothing
  if not res
    then  return FailedChange
    else if any (name `L.isInfixOf`) (lines containers)
            then return MadeChange
            else toResult <$> C.boolSystem "docker" (map C.Params [ "create","--name=" <> name, " ", image
                                                                  , "echo", "'data container for capitalmatch app'"])
