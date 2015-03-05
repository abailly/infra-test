module Capital.Property.Lending (lendingHost) where

import           Capital.Property.Docker     (installLatestDocker)
import           Capital.Property.Locale
import           Propellor
import           Propellor.CmdLine
import qualified Propellor.Property.File     as File
import qualified Propellor.Property.Firewall as Firewall
import           System.Posix.Files
import           Utility.FileMode

firewallHttpsDockerSsh :: Property HasInfo
firewallHttpsDockerSsh = propertyList "creating firewall for ssh, http(s) and docker" $ prop
        & Firewall.installed
        & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
        & Firewall.rule INPUT ACCEPT (IFace "lo")
        & Firewall.rule INPUT ACCEPT (IFace "docker0")
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 22)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 80)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 443)
        & Firewall.rule INPUT DROP   Everything

lendingHost :: Property HasInfo
lendingHost = propertyList "creating devserver configuration" $ prop
        -- ipv4 takes precedence over ipv6 on ipv6 enabled host
	-- https://www.digitalocean.com/community/questions/how-to-disable-ubuntu-14-04-ipv6
	& File.containsLine "/etc/gai.conf" "precedence ::ffff:0:0/96 100"
	& setDefaultLocale en_us_UTF_8
        & firewallHttpsDockerSsh
	& installLatestDocker
        & File.dirExists "/etc/nginx/conf/certs/"
	& withPrivData (PrivFile "nginx-private-key") (Context "lending.capital-match.com")
	  (\ getdata -> property "setting nginx private key"
					$ getdata $ \ tok -> liftIO $ ((writeFile "/etc/nginx/conf/certs/ssl.key" tok) >> return MadeChange) `catchIO` const (return FailedChange))
	& "/etc/nginx/conf/certs/ssl.key" `File.mode` combineModes [ownerWriteMode, ownerReadMode]

	& withPrivData (PrivFile "nginx-public-cert") (Context "lending.capital-match.com")
	  (\ getdata -> property "setting nginx certificate chain"
					$ getdata $ \ tok -> liftIO $ ((writeFile "/etc/nginx/conf/ssl-unified.crt" tok) >> return MadeChange) `catchIO` const (return FailedChange))
        & installLatestDocker
        & User.accountFor "build"
          & User.hasGroup "build" "docker"
	  & Sudo.binaryEnabledFor "/usr/bin/docker" "build"
 		& Ssh.keyImported SshRsa "build" (Context "beta.capital-match.com")
		  & File.containsLines "/home/build/.ssh/config"
		  [ "Host bitbucket.org"
				  , "\tUser git"
				  , "\tHostname bitbucket.org"
				  , "\tPreferredAuthentications publickey"
				  , "\tIdentityFile \"/home/build/.ssh/id_rsa\""
				  ]
		  & Ssh.knownExternalHost "bitbucket.org" "build"
		  & Ssh.authorizedKeys "build" (Context "beta.capital-match.com") -- TODO disable or have separate keys for production

          & File.hasContent "/home/build/startnginx.sh"
          ["#!/bin/sh"
          ,"#set -x"
          ,"#set -e"
          , "if [-f /home/build/.app.cid ]; then"
          ,"    docker kill $(cat /home/build/.app.cid)"
          ,"    rm /home/build/.app.cid"
          ,"  fi"
          ,"  # run as build user"
          ,"  docker run -d --cidfile=/home/build/.app.cid -p 8080:8080 -v /home/build/data:/data capital/app:latest"
          ,"  if [ -f /home/build/.nginx.cid ]; then"
          ,"    docker kill $(cat /home/build/.nginx.cid)"
          ,"    rm /home/build/.nginx.cid"
          ,"  fi"
          ,"  # clone or pull nginx config as build user"
          ,"  export NGINXCONF=/home/build/nginxconf/nginx"
          ,"  if [ -d /home/build/nginxconf ]; then"
          ,"    cd /home/build/nginxconf && git pull origin master"
          ,"  else "
          ,"    cd /home/build && git clone capital-match nginxconf"
          ,"  fi","  docker run -d --cidfile=/home/build/.nginx.cid -p 80:80 -p 443:443 -v $NGINXCONF/nginx.conf:/etc/nginx/nginx.conf -v $NGINXCONF/sites-enabled:/etc/nginx/sites-enabled -v $NGINXCONF/certs:/etc/nginx/certs -v $NGINXCONF/logs:/var/log/nginx capital/nginx"]
		  & File.mode "/home/build/startnginx.sh" (combineModes  (ownerWriteMode:readModes ++ executeModes))
      & File.ownerGroup "/home/build/startnginx.sh" "build" "build"


