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
--import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
--import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
--import qualified Propellor.Property.Hostname as Hostname
--import qualified Propellor.Property.Reboot as Reboot
--import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.Git as Git

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
-- Edit this to configure propellor!
hosts :: [Host]
hosts =
	[ host "beta.capital-match.com"
          & Cron.runPropellor "30 * * * *"
          & Apt.serviceInstalledRunning "nginx"
          & Git.installed
          -- configure user build
          & User.accountFor "build"

        , host "92.243.3.60"
          & Apt.serviceInstalledRunning "apache2"
          & Apache.modEnabled "ssl"
          & propertyList "atdd.io site"  [
            File.ownerGroup "/srv/nono-data/atdd.io/_site" "admin" "admin"
            ,"/srv/nono-data/atdd.io/_site" `File.mode` combineModes [ownerWriteMode, ownerReadMode, ownerExecuteMode, groupReadMode, groupExecuteMode]
            ,toProp $ Apache.siteEnabled "atdd.io" $ apachecfg "atdd.io" "/srv/nono-data/atdd.io/_site" NoSSL []
            ]


          -- A generic webserver in a Docker container.
	, Docker.container "webserver" "joeyh/debian-stable"
		& os (System (Debian Stable) "amd64")
		& Apt.stdSourcesList
		& Docker.publish "80:80"
		& Docker.volume "/var/www:/var/www"
		& Apt.serviceInstalledRunning "apache2"

	-- add more hosts here...
	--, host "foo.example.com" = ...
	]


data VHostSSL = NoSSL
              | WithSSL
              deriving (Eq,Show,Read)
                       
-- | Configuration for apache virtual host
-- stolen from JoeysSites
apachecfg :: HostName             -- ^Host's name
          -> FilePath            -- ^Path to document root
          -> VHostSSL            -- ^Configure SSL virtual host?
          -> Apache.ConfigFile   -- ^Configuration file to modify
          -> Apache.ConfigFile
apachecfg hn documentRoot withSSL middle
  | withSSL == WithSSL = vhost NoSSL  ++ vhost WithSSL
  | otherwise         = vhost NoSSL
  where
	vhost ssl = 
		[ "<VirtualHost *:"++show port++">"
		, "  ServerAdmin arnaud@foldlabs.com"
		, "  ServerName "++hn++":"++show port
                , "  DocumentRoot " ++ documentRoot
                , "  <Directory " ++ documentRoot ++ ">"
		, "    Options Indexes FollowSymlinks MultiViews"
		, "    AllowOverride None"
		, "    Order allow,deny"
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

mainhttpscert :: VHostSSL -> Apache.ConfigFile
mainhttpscert NoSSL   = []
mainhttpscert WithSSL = 
	[ "  SSLEngine on"
	, "  SSLCertificateFile /etc/ssl/certs/web.pem"
	, "  SSLCertificateKeyFile /etc/ssl/private/web.pem"
	, "  SSLCertificateChainFile /etc/ssl/certs/startssl.pem"
	]
		
