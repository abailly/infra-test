module Propellor.Property.Group where

import Propellor

type GID = Int

exists :: GroupName -> Maybe GID -> Property NoInfo
exists group' mgid = check test (cmdProperty "addgroup" $ args mgid)
	`describe` unwords ["group", group']
  where
	groupFile = "/etc/group"
	test = not . elem group' . words <$> readProcess "cut" ["-d:", "-f1", groupFile]
	args Nothing = [group']
	args (Just gid) = ["--gid", show gid, group']
