-- |Properties for configuring firewall (iptables) rules
--
-- Copyright 2014 Arnaud Bailly <arnaud.oqube@gmail.com>
-- License: BSD-2-Clause
module Propellor.Property.Firewall (
  rule, flush,
  installed,
  dropEverything,
  Chain(..),
  Target(..),
  Proto(..),
  Rules(..),
  Port,
  ConnectionState(..)
  ) where

import           Data.Char
import           Data.List
import           Data.Monoid

import           Propellor
import qualified Propellor.Property.Apt     as Apt
import qualified Propellor.Property.Network as Network
import           Utility.SafeCommand

installed :: Property NoInfo
installed = Apt.installed ["iptables"]

-- |A basic rule to drop every input packet
--
-- This should be used as last clause for a bunch of rules, like:
dropEverything :: Property NoInfo
dropEverything =  rule INPUT DROP  Everything

-- |Drop all rules for given chain.
--
-- Useful at start of configuration of firewall rules
flush :: Chain -> Property NoInfo
flush chain = property ( "flushing all rules for chain " <> show chain) $ liftIO $
  toResult <$> boolSystem "iptables" (map Param ["-F", show chain])

rule :: Chain -> Target -> Rules -> Property NoInfo
rule c t rs = property ("firewall rule: " <> show r) addIpTable
  where
	r = Rule c t rs
	addIpTable = liftIO $ do
		let args = toIpTable r
		(_, exist) <- processTranscript "iptables" ("-C" : toCommand args) Nothing
		if exist
                  then return NoChange
                  else toResult <$> boolSystem "iptables" (add args)
	add params = Param "-A" : params

toIpTable :: Rule -> [CommandParam]
toIpTable r =  map Param $
	show (ruleChain r) :
	toIpTableArg (ruleRules r) ++ [ "-j" , show $ ruleTarget r ]

toIpTableArg :: Rules -> [String]
toIpTableArg Everything        = []
toIpTableArg (Proto proto)     = ["-p", map toLower $ show proto]
toIpTableArg (Port port)       = ["--dport", show port]
toIpTableArg (PortRange (f,t)) = ["--dport", show f ++ ":" ++ show t]
toIpTableArg (IFace iface)     = ["-i", iface]
toIpTableArg (Ctstate states)  = [ "-m"
                                 , "conntrack"
                                 , "--ctstate"
                                 , intercalate "," (map show states)
                                 ]
toIpTableArg (r :- r')         = toIpTableArg r <> toIpTableArg r'

data Rule = Rule { ruleChain  :: Chain
                 , ruleTarget :: Target
                 , ruleRules  :: Rules
                 } deriving (Eq, Show, Read)

data Chain = INPUT | OUTPUT | FORWARD
	deriving (Eq,Show,Read)

data Target = ACCEPT | REJECT | DROP | LOG
	deriving (Eq,Show,Read)

data Proto = TCP | UDP | ICMP
	deriving (Eq,Show,Read)

type Port = Int

data ConnectionState = ESTABLISHED | RELATED | NEW | INVALID
	deriving (Eq,Show,Read)

data Rules
	= Everything
	| Proto Proto
	-- ^There is actually some order dependency between proto and port so this should be a specific
	-- data type with proto + ports
	| Port Port
	| PortRange (Port,Port)
	| IFace Network.Interface
	| Ctstate [ ConnectionState ]
	| Rules :- Rules   -- ^Combine two rules
	deriving (Eq,Show,Read)

infixl 0 :-

instance Monoid Rules where
	mempty  = Everything
	mappend = (:-)
