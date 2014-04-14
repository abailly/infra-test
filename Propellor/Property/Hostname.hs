module Propellor.Property.Hostname where

import Propellor
import qualified Propellor.Property.File as File

-- | Ensures that the hostname is set to the HostAttr value.
-- Configures /etc/hostname and the current hostname.
--
-- When the hostname is a FQDN, also configures /etc/mailname
-- with the domain part.
-- A FQDN also configures /etc/hosts, with an entry for 127.0.1.1, which is
-- standard at least on Debian to set the FDQN (127.0.0.1 is localhost).
sane :: Property
sane = Property ("sane hostname") (ensureProperty . setTo =<< getHostName)

setTo :: HostName -> Property
setTo hn = combineProperties desc go
	`onChange` cmdProperty "hostname" [basehost]
  where
	desc = "hostname " ++ hn
	(basehost, domain) = separate (== '.') hn

	go = catMaybes
		[ Just $ "/etc/hostname" `File.hasContent` [basehost]
		, if null domain
			then Nothing 
			else Just $ File.fileProperty desc
				addhostline "/etc/hosts"
		, if null domain
			then Nothing
			else Just $ "/etc/mailname" `File.hasContent` [domain]
		]
	
	hostip = "127.0.1.1"
	hostline = hostip ++ "\t" ++ hn ++ " " ++ basehost

	addhostline ls = hostline : filter (not . hashostip) ls
	hashostip l = headMaybe (words l) == Just hostip
