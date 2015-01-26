module Propellor.Property.Network where

import Propellor
import Propellor.Property.File

type Interface = String

ifUp :: Interface -> Property NoInfo
ifUp iface = cmdProperty "ifup" [iface]

-- | Resets /etc/network/interfaces to a clean and empty state,
-- containing just the standard loopback interface, and with
-- interfacesD enabled.
--
-- This can be used as a starting point to defining other interfaces.
--
-- No interfaces are brought up or down by this property.
cleanInterfacesFile :: Property NoInfo
cleanInterfacesFile = hasContent interfacesFile
	[ "# Deployed by propellor, do not edit."
	, ""
	, "source-directory interfaces.d"
	, ""
	, "# The loopback network interface"
	, "auto lo"
	, "iface lo inet loopback"
	]
	`describe` ("clean " ++ interfacesFile)

-- | Writes a static interface file for the specified interface.
--
-- The interface has to be up already. It could have been brought up by
-- DHCP, or by other means. The current ipv4 addresses
-- and routing configuration of the interface are written into the file.
--
-- If the interface file already exists, this property does nothing,
-- no matter its content.
-- 
-- (ipv6 addresses are not included because it's assumed they come up
-- automatically in most situations.)
static :: Interface -> Property NoInfo
static iface = check (not <$> doesFileExist f) setup
	`describe` desc
	`requires` interfacesDEnabled
  where
	f = interfaceDFile iface
	desc = "static " ++ iface
	setup = property desc $ do
		ls <- liftIO $ lines <$> readProcess "ip"
			["-o", "addr", "show", iface, "scope", "global"]
		stanzas <- liftIO $ concat <$> mapM mkstanza ls
		ensureProperty $ hasContent f $ ("auto " ++ iface) : stanzas
	mkstanza ipline = case words ipline of
		-- Note that the IP address is written CIDR style, so
		-- the netmask does not need to be specified separately.
		(_:iface':"inet":addr:_) | iface' == iface -> do
			gw <- getgateway
			return $ catMaybes
				[ Just $ "iface " ++ iface ++ " inet static"
				, Just $ "\taddress " ++ addr
				, ("\tgateway " ++) <$> gw
				]
		_ -> return []
	getgateway = do
		rs <- lines <$> readProcess "ip"
			["route", "show", "scope", "global", "dev", iface]
		return $ case words <$> headMaybe rs of
			Just ("default":"via":gw:_) -> Just gw
			_ -> Nothing

-- | 6to4 ipv6 connection, should work anywhere
ipv6to4 :: Property NoInfo
ipv6to4 = hasContent (interfaceDFile "sit0")
	[ "# Deployed by propellor, do not edit."
	, "iface sit0 inet6 static"
	, "\taddress 2002:5044:5531::1"
	, "\tnetmask 64"
	, "\tgateway ::192.88.99.1"
	, "auto sit0"
	]
	`describe` "ipv6to4"
	`requires` interfacesDEnabled
	`onChange` ifUp "sit0"

interfacesFile :: FilePath
interfacesFile = "/etc/network/interfaces"

-- | A file in the interfaces.d directory.
interfaceDFile :: Interface -> FilePath
interfaceDFile iface = "/etc/network/interfaces.d" </> iface

-- | Ensures that files in the the interfaces.d directory are used.
interfacesDEnabled :: Property NoInfo
interfacesDEnabled = containsLine interfacesFile "source-directory interfaces.d"
	`describe` "interfaces.d directory enabled"
