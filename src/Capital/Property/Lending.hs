module Capital.Property.Lending (lendingHost) where

import           Propellor

import           System.Posix.Files
import           Utility.FileMode


import           Capital.Property.Docker   (createImage, dockerAuthTokenFor,
                                            hasDataContainer,
                                            installLatestDocker)
import           Capital.Property.Firewall (firewallHttpsDockerSsh)
import           Capital.Property.Locale
import qualified Propellor.Property.Cmd    as Cmd
import qualified Propellor.Property.File   as File
import qualified Propellor.Property.Ssh    as Ssh
import qualified Propellor.Property.Sudo   as Sudo
import qualified Propellor.Property.User   as User

-- | ipv4 takes precedence over ipv6 on ipv6 enabled host
-- This causes problems for docker networking
-- https://www.digitalocean.com/community/questions/how-to-disable-ubuntu-14-04-ipv6
fixDigitalOceanIpV6  :: Property NoInfo
fixDigitalOceanIpV6 = File.containsLine "/etc/gai.conf" "precedence ::ffff:0:0/96 100"

lendingHost :: Property HasInfo
lendingHost = propertyList "creating lending.capital-match.com configuration" $ props
    & fixDigitalOceanIpV6
    & setDefaultLocale en_us_UTF_8
    & firewallHttpsDockerSsh
    & installLatestDocker
    & File.dirExists certPath

    & writeSslKey "nginx-private-key" "lending.capital-match.com"
    & restrictToOwner certPath "ssl.key"

    & writeCertificateChain "nginx-public-cert" "lending.capital-match.com"
    & (certPath <> "ssl-unified.crt") `File.mode` combineModes [ownerWriteMode, ownerReadMode]
    & User.accountFor "build"
    & User.hasGroup "build" "docker"
    & Sudo.binaryEnabledFor "/usr/bin/docker" "build"
    & Ssh.authorizedKeys "build" (Context "beta.capital-match.com") -- TODO disable or have separate keys for production
    & dockerAuthTokenFor "build"
    & File.dirExists nginxSitesPath
    & fileHasContentsFrom "lending/server" (nginxSitesPath </> "server")
    & fileHasContentsFrom "lending/startnginx.sh" (buildHome </> "startnginx.sh")
    & createImage dataImage "lending/cmdata-image"
    & hasDataContainer "cm-data" dataImage
    & Cmd.userScriptProperty deployer ["./startnginx.sh"]

  where
     deployer = "build"
     buildHome = "/home/build/"
     nginxSitesPath = buildHome <> "nginxconf/sites-enabled"
     certPath = "/etc/nginx/certs/"
     restrictToOwner path fileName = (path <> fileName) `File.mode` combineModes [ownerWriteMode, ownerReadMode]
     dataImage = "capitalatch/data:1.0"
     writeToCertPath fileName tok = (writeFile (certPath <> fileName) tok >> return MadeChange) `catchIO` const (return FailedChange)
     writeSslKey privFile context = writePrivFile privFile context "ssl.key" "setting nginx private key"
     writeCertificateChain privFile context = writePrivFile privFile context "ssl-unified.crt" "setting nginx certificate chain"
     writePrivFile privFile context destination comment =
       withPrivData (PrivFile privFile) (Context context)
         (\ getdata -> property comment
			$ getdata $ \ tok -> liftIO $ writeToCertPath destination tok)


fileHasContentsFrom :: FilePath -> FilePath -> Property NoInfo
fileHasContentsFrom source target = property  ("setting file " <> target <> " to " <> source ) $ liftIO $ (copyFile (sourcePath <> source) target >> return MadeChange ) `catchIO` const (return FailedChange)
  where sourcePath = "pubdata/"

{- put nginx file
      & File.ownerGroup "/home/build/startnginx.sh" "build" "build" -}
