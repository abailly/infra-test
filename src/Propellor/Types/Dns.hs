module Propellor.Types.Dns where

import Propellor.Types.OS (HostName)
import Propellor.Types.Empty

import Data.Word
import Data.Monoid
import qualified Data.Map as M

type Domain = String

data IPAddr = IPv4 String | IPv6 String
	deriving (Read, Show, Eq, Ord)

fromIPAddr :: IPAddr -> String
fromIPAddr (IPv4 addr) = addr
fromIPAddr (IPv6 addr) = addr

-- | Represents a bind 9 named.conf file.
data NamedConf = NamedConf
	{ confDomain :: Domain
	, confDnsServerType :: DnsServerType
	, confFile :: FilePath
	, confMasters :: [IPAddr]
	, confAllowTransfer :: [IPAddr]
	, confLines :: [String]
	}
	deriving (Show, Eq, Ord)

data DnsServerType = Master | Secondary
	deriving (Show, Eq, Ord)

-- | Represents a bind 9 zone file.
data Zone = Zone
	{ zDomain :: Domain
	, zSOA :: SOA
	, zHosts :: [(BindDomain, Record)]
	}
	deriving (Read, Show, Eq)

-- | Every domain has a SOA record, which is big and complicated.
data SOA = SOA
	{ sDomain :: BindDomain
	-- ^ Typically ns1.your.domain
	, sSerial :: SerialNumber
	-- ^ The most important parameter is the serial number,
	-- which must increase after each change.
	, sRefresh :: Integer
	, sRetry :: Integer
	, sExpire :: Integer
	, sNegativeCacheTTL :: Integer
	}
	deriving (Read, Show, Eq)

-- | Types of DNS records.
--
-- This is not a complete list, more can be added.
data Record
	= Address IPAddr
	| CNAME BindDomain
	| MX Int BindDomain
	| NS BindDomain
	| TXT String
	| SRV Word16 Word16 Word16 BindDomain
	| SSHFP Int Int String
	| INCLUDE FilePath
	deriving (Read, Show, Eq, Ord)

getIPAddr :: Record -> Maybe IPAddr
getIPAddr (Address addr) = Just addr
getIPAddr _ = Nothing

getCNAME :: Record -> Maybe BindDomain
getCNAME (CNAME d) = Just d
getCNAME _ = Nothing

getNS :: Record -> Maybe BindDomain
getNS (NS d) = Just d
getNS _ = Nothing

-- | Bind serial numbers are unsigned, 32 bit integers.
type SerialNumber = Word32

-- | Domains in the zone file must end with a period if they are absolute.
--
-- Let's use a type to keep absolute domains straight from relative
-- domains.
--
-- The RootDomain refers to the top level of the domain, so can be used
-- to add nameservers, MX's, etc to a domain.
data BindDomain = RelDomain Domain | AbsDomain Domain | RootDomain
	deriving (Read, Show, Eq, Ord)

domainHostName :: BindDomain -> Maybe HostName
domainHostName (RelDomain d) = Just d
domainHostName (AbsDomain d) = Just d
domainHostName RootDomain = Nothing

newtype NamedConfMap = NamedConfMap (M.Map Domain NamedConf)
	deriving (Eq, Ord, Show)

-- | Adding a Master NamedConf stanza for a particulr domain always
-- overrides an existing Secondary stanza for that domain, while a
-- Secondary stanza is only added when there is no existing Master stanza.
instance Monoid NamedConfMap where
	mempty = NamedConfMap M.empty
	mappend (NamedConfMap old) (NamedConfMap new) = NamedConfMap $
		M.unionWith combiner new old
	  where
		combiner n o = case (confDnsServerType n, confDnsServerType o) of
			(Secondary, Master) -> o
			_  -> n

instance Empty NamedConfMap where
	isEmpty (NamedConfMap m) = isEmpty m

fromNamedConfMap :: NamedConfMap -> M.Map Domain NamedConf
fromNamedConfMap (NamedConfMap m) = m
