module Capital.Property.Lending (lendingHost) where

import           Propellor
import           Propellor.CmdLine
import           Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Git      as Git
-- import Propellor.Property.Scheduled
import qualified Propellor.Property.File     as File
import           System.Posix.Files
import           Utility.FileMode

lendingHost :: Property HasInfo
lendingHost = propertyList "creating devserver configuration" $ props
		  -- ipv4 takes precedence over ipv6 on ipv6 enabled host
		  -- https://www.digitalocean.com/community/questions/how-to-disable-ubuntu-14-04-ipv6
		  & File.containsLine "/etc/gai.conf" "precedence ::ffff:0:0/96 100"
--		   & setDefaultLocale en_us_UTF_8
--		  & installLatestDocker
