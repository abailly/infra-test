module Propellor.Types.CmdLine where

import Propellor.Types.OS
import Propellor.Types.PrivData

import System.Posix.Types

data CmdLine
	= Run HostName
	| Spin [HostName] (Maybe HostName)
	| SimpleRun HostName
	| Set PrivDataField Context
	| Dump PrivDataField Context
	| Edit PrivDataField Context
	| ListFields
	| AddKey String
	| Merge
	| Serialized CmdLine
	| Continue CmdLine
	| Update (Maybe HostName)
	| Relay HostName
	| DockerInit HostName
	| DockerChain HostName String
	| ChrootChain HostName FilePath Bool Bool
	| GitPush Fd Fd
	deriving (Read, Show, Eq)

