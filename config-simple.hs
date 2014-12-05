-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import Propellor.CmdLine
-- import Propellor.Property.Scheduled
import Utility.FileMode
import System.Posix.Files


import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Apache as Apache
-- import qualified Propellor.Property.Network as Network
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
--import qualified Propellor.Property.Hostname as Hostname
--import qualified Propellor.Property.Reboot as Reboot
--import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Git as Git
import Propellor.Property.Firewall as Firewall
import Propellor.Property.Fig as Fig

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
-- Edit this to configure propellor!
hosts :: [Host]
hosts =
	[ host "beta.capital-match.com"
          & Cron.runPropellor "30 * * * *"
          & Git.installed
          & Docker.installed
		  & Fig.installed
          -- configure user build
          & User.accountFor "build"
		  & Ssh.keyImported SshRsa "build" (Context "beta.capital-match.com")
		  & File.containsLines "/home/build/.ssh/config"
		  [ "Host bitbucket.org"
				  , "\tUser git"
				  , "\tHostname bitbucket.org"
				  , "\tPreferredAuthentications publickey"
				  , "\tIdentityFile \"/home/build/.ssh/id_rsa\""
				  ]
		  & Ssh.knownExternalHost "bitbucket.org" "build"
		  & Ssh.authorizedKeys "build" (Context "beta.capital-match.com")
		  & Sudo.binaryEnabledFor "/usr/bin/docker" "build"
		  & Git.clonedBare "build" "git@bitbucket.org:abailly/capital-match.git" "/home/build/capital-match.git"
		  & File.hasContent "/home/build/capital-match.git/hooks/post-receive"
		  [ "#!/bin/sh"
				  , "read START STOP BRANCH"
				  , "echo \"branch: $BRANCH\""
				  , "expr \"$BRANCH\" : '.*/master' || exit 0"
				  , "CHANGED=$(git diff --name-status $START..$STOP)"
				  , "WORK_DIR=/home/build/capital-match/"
				  , "GIT_WORK_TREE=${WORK_DIR} git checkout -f"
				  , "cd $WORK_DIR"
				  , "if echo $CHANGED | grep \"capital-match.cabal\\|project.clj\\|deps/\"  > /dev/null 2>&1 ; then"
				  , "  cp capital-match.cabal deps/ "
				  , "  cp ui/project.clj  deps/ "
				  , "  sudo docker build -t capital/deps deps || exit 1"
				  , "fi"
				  , "sudo docker build -t capital/app .  || exit 1"
				  , "CID_FILE=/home/build/.app.cid"
				  , "if [ -f $CID_FILE ]; then"
				  , "  sudo docker kill $(cat $CID_FILE)"
				  , "  rm $CID_FILE"
				  , "fi"
				  , "sudo docker run -d --cidfile=$CID_FILE -p 80:8080 -v /home/build/data:/data capital/app:latest"
				  ]
		  & File.mode "/home/build/capital-match.git/hooks/post-receive" (combineModes  (ownerWriteMode:readModes ++ executeModes))
		  
		, host "dev.capital-match.com"
				  & Git.installed
				  & Docker.installed
				  & Fig.installed
				  -- configure user build
				  & User.accountFor "build"
				  & Ssh.keyImported SshRsa "build" (Context "beta.capital-match.com")
				  & File.containsLines "/home/build/.ssh/config"
				  [ "Host bitbucket.org"
								  , "\tUser git"
								  , "\tHostname bitbucket.org"
								  , "\tPreferredAuthentications publickey"
								  , "\tIdentityFile \"/home/build/.ssh/id_rsa\""
								  ]
				  & Ssh.knownExternalHost "bitbucket.org" "build"
				  & Ssh.authorizedKeys "build" (Context "beta.capital-match.com")
				  & Sudo.binaryEnabledFor "/usr/bin/docker" "build"

        , host "test.atdd.io"
		  & Docker.installed
		  & setDefaultLocale en_us_UTF_8
		  & Git.installed
          & User.accountFor "admin"
          & Sudo.binaryEnabledFor "/usr/bin/docker" "admin"
          & Ssh.authorizedKeys "admin" (Context "test.atdd.io")
		  -- keys for pulling from private bitbucket repo
		  & Ssh.keyImported SshRsa "admin" (Context "test.atdd.io")
		  & File.containsLines "/home/admin/.ssh/config"
		  [ "Host bitbucket.org"
				  , "\tUser git"
				  , "\tHostname bitbucket.org"
				  , "\tPreferredAuthentications publickey"
				  , "\tIdentityFile \"/home/admin/.ssh/id_rsa\""
				  ]
		  & Ssh.knownExternalHost "bitbucket.org" "admin"
		  -- fig
		  & Fig.installed
          & Firewall.installed
          & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
          & Firewall.rule INPUT ACCEPT (IFace "lo")
          & Firewall.rule INPUT ACCEPT (IFace "docker0")
          & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 22)
          & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 80)
          & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 443)
          & Firewall.rule INPUT DROP   Everything
          
        , host "brightbox"
          & User.accountFor "admin"

        , host "92.243.3.60"
          & Git.installed
          & setDefaultLocale en_us_UTF_8
          & Apt.serviceInstalledRunning "apache2"
          & Apache.modEnabled "ssl"
          & User.accountFor "admin"
          & standardHakyllSite "atdd.io" ["www.atdd.io"]
          & standardHakyllSite "bailly.me" []
          & standardHakyllSite "blog.foldlabs.com" []

        -- new systemsthinking.net
        , host "advandenende.eu"
              & Apt.serviceInstalledRunning "apache2"
              & Apt.serviceInstalledRunning "mariadb"
              & User.accountFor "admin"
              & Apt.installed ["php5","libapache2-mod-php5","php5-mysql","locales"]

          & Docker.installed
          & setDefaultLocale en_us_UTF_8
          & Git.installed
          & User.accountFor "admin"
          & Sudo.binaryEnabledFor "/usr/bin/docker" "admin"
          & Firewall.installed
          & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
          & Firewall.rule INPUT ACCEPT (IFace "lo")
          & Firewall.rule INPUT ACCEPT (IFace "docker0")
          & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 22)
          & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 80)
          & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 443)
          & Firewall.rule INPUT DROP   Everything
               
	--, host "foo.example.com" = ...
	]

-- | Configures a hakyll-generated site as a vhost served by apache
standardHakyllSite :: HostName -> [ HostName ] -> Property
standardHakyllSite siteName aliases = propertyList ("serving " ++ siteName ++ " site")  [
  File.ownerGroup directory "admin" "admin"
  ,directory `File.mode` combineModes [ownerWriteMode, ownerReadMode, ownerExecuteMode, groupReadMode, groupExecuteMode]
  ,toProp $ Apache.siteEnabled siteName $ apachecfg siteName aliases directory NoSSL []
  ]
  where
    directory = "/srv/nono-data/" ++ siteName ++ "/_site"

data VHostSSL = NoSSL
              | WithSSL
              deriving (Eq,Show,Read)
                       
-- | Configuration for apache virtual host
-- stolen from JoeysSites
apachecfg :: HostName             -- ^Host's name
          -> [HostName]             -- ^Host's name
          -> FilePath            -- ^Path to document root
          -> VHostSSL            -- ^Configure SSL virtual host?
          -> Apache.ConfigFile   -- ^Configuration file to modify
          -> Apache.ConfigFile
apachecfg hn aliases documentRoot withSSL middle
  | withSSL == WithSSL = vhost NoSSL  ++ vhost WithSSL
  | otherwise         = vhost NoSSL
  where
	vhost ssl = 
		[ "<VirtualHost *:"++show port++">"
		, "  ServerAdmin arnaud@foldlabs.com"
		, "  ServerName "++hn++":"++show port
                ]
                ++ map serverAlias aliases ++
                [
                  "  DocumentRoot " ++ documentRoot
                , "  <Directory " ++ documentRoot ++ ">"
		, "    Options Indexes FollowSymlinks MultiViews"
		, "    AllowOverride None"
		, "    Order allow,deny"
                , "    Allow from all"
		, "  </Directory>"
		, "  ScriptAlias /cgi-bin/ /usr/lib/cgi-bin/"
		, "  <Directory /usr/lib/cgi-bin>"
		, "    SetHandler cgi-script"
		, "    Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch"
		, "  </Directory>"

		]
		++ mainhttpscert ssl
		++ middle ++
		[ ""
                , " ErrorLog ${APACHE_LOG_DIR}/"++ hn ++ ".error.log"
                , " LogLevel warn"
                , " CustomLog ${APACHE_LOG_DIR}/"++hn++".access.log combined"
		, "  ServerSignature On"
		, "  "
		, "  <Directory \"/usr/share/apache2/icons\">"
		, "      Options Indexes MultiViews"
		, "      AllowOverride None"
		, "      Order allow,deny"
		, "      Allow from all"
		, "  </Directory>"
		, "</VirtualHost>"
		]
	  where
		port = case withSSL of
                        NoSSL   -> 80 :: Int
                        WithSSL -> 443 :: Int
                serverAlias hostname = "  ServerAlias " ++ hostname

mainhttpscert :: VHostSSL -> Apache.ConfigFile
mainhttpscert NoSSL   = []
mainhttpscert WithSSL = 
	[ "  SSLEngine on"
	, "  SSLCertificateFile /etc/ssl/certs/web.pem"
	, "  SSLCertificateKeyFile /etc/ssl/private/web.pem"
	, "  SSLCertificateChainFile /etc/ssl/certs/startssl.pem"
	]
		

data Lang = En
          | Fr

instance Show Lang where
  show En = "en"
  show Fr = "fr"
   
data Country = US
             | FR
             deriving (Show)


data Encoding = UTF_8

instance Show Encoding where
  show UTF_8 = "UTF-8"
  
data Locale = SimpleLocale Lang Country
            | EncodingLocale Lang Country Encoding

instance Show Locale where
  show (SimpleLocale l c)     = show l ++ "_" ++ show c
  show (EncodingLocale l c e) = show l ++ "_" ++ show c ++ "."++ show e

en_us_UTF_8 :: Locale
en_us_UTF_8 = EncodingLocale En US UTF_8

setDefaultLocale :: Locale -> Property
setDefaultLocale locale = propertyList ("setting default locale to " ++ localeString) [
  Apt.installed ["locales"]
  , scriptProperty [ "locale-gen " ++ localeString ]
  , "/etc/default/locale" `File.hasContent` [
    "LC_ALL=" ++ localeString
    ,"LANG=" ++  localeString
    ]
  ]
  where
    localeString = show locale 
