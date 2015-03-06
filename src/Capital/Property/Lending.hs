module Capital.Property.Lending (lendingHost) where

import           Propellor
import           System.Posix.Files
import           Utility.FileMode


import           Capital.Property.Docker   (installLatestDocker)
import           Capital.Property.Firewall (firewallHttpsDockerSsh)
import           Capital.Property.Locale
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
lendingHost = propertyList "creating lending.capital-match.com configuration" $ do
  props
    & fixDigitalOceanIpV6
    & setDefaultLocale en_us_UTF_8
    & firewallHttpsDockerSsh
    & installLatestDocker
    & File.dirExists certPath
    & writeSslKey "nginx-private-key" "lending.capital-match.com"
    & (certPath <> "ssl.key") `File.mode` combineModes [ownerWriteMode, ownerReadMode]

    & writeCertificateChain "nginx-public-cert" "lending.capital-match.com"
    & (certPath <> "ssl-unified.crt") `File.mode` combineModes [ownerWriteMode, ownerReadMode]
    & installLatestDocker
    & User.accountFor "build"
      & User.hasGroup "build" "docker"
      & Sudo.binaryEnabledFor "/usr/bin/docker" "build"
    	  & Ssh.knownExternalHost "bitbucket.org" "build"
    	  & Ssh.authorizedKeys "build" (Context "beta.capital-match.com") -- TODO disable or have separate keys for production
   where
     certPath = "/etc/nginx/certs/"
     writeToCertPath fileName tok =
       (writeFile (certPath <> fileName) tok >> return MadeChange) `catchIO` const (return FailedChange)
     writeSslKey privFile context = writePrivFile privFile context "setting nginx private key" "ssl.key"
     writeCertificateChain privFile context = writePrivFile privFile context "setting nginx certificate chain" "ssl-unified.crt"
     writePrivFile privFile context destination comment =
       withPrivData (PrivFile privFile) (Context context)
         (\ getdata -> property comment
			$ getdata $ \ tok -> liftIO $ writeToCertPath destination tok)



{- put nginx file
      & File.ownerGroup "/home/build/startnginx.sh" "build" "build" -}
