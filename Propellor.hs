import Common
import CmdLine
import qualified Property.File as File
import qualified Property.Apt as Apt
import qualified Property.Ssh as Ssh
import qualified Property.Sudo as Sudo
import qualified Property.User as User
import qualified Property.Hostname as Hostname
import qualified Property.Reboot as Reboot
import qualified Property.Tor as Tor
import qualified Property.Docker as Docker
import qualified Property.GitHome as GitHome

main :: IO ()
main = defaultMain getProperties

{- This is where the system's HostName, either as returned by uname
 - or one specified on the command line, is converted into a list of
 - Properties for that system. -}
getProperties :: HostName -> [Property]
getProperties hostname@"clam.kitenet.net" =
	[ cleanCloudAtCost hostname
	, standardSystem Apt.Unstable
	-- Clam is a tor bridge.
	, Tor.isBridge
	-- I play with docker on clam.
	, Docker.configured
	-- This is not an important system so I don't want to need to 
	-- manually upgrade it.
	, Apt.unattendedUpgrades True
	-- Should come last as it reboots.
	, Apt.installed ["systemd-sysv"] `onChange` Reboot.now
	]
-- add more hosts here...
--getProperties "foo" =
getProperties h = error $ unwords
	[ "Unknown host:", h
	, "(perhaps you should specify the real hostname on the command line?)"
	]

-- This is my standard system setup
standardSystem :: Apt.Suite -> Property
standardSystem suite = propertyList "standard system"
	[ Apt.stdSourcesList suite `onChange` Apt.upgrade
	, Apt.installed ["etckeeper"]
	, Apt.installed ["ssh"]
	, GitHome.installedFor "root"
	, User.hasSomePassword "root"
	-- Harden the system, but only once root's authorized_keys
	-- is safely in place.
	, check (Ssh.hasAuthorizedKeys "root") $
		Ssh.passwordAuthentication False
	, User.sshAccountFor "joey"
	, User.hasSomePassword "joey"
	, Sudo.enabledFor "joey"
	, GitHome.installedFor "joey"
	, Apt.installed ["vim", "screen"]
	-- I use postfix, or no MTA.
	, Apt.removed ["exim4"] `onChange` Apt.autoRemove
	]

-- Clean up a system as installed by cloudatcost.com
cleanCloudAtCost :: HostName -> Property
cleanCloudAtCost hostname = propertyList "cloudatcost cleanup"
	[ Hostname.set hostname
	, Ssh.uniqueHostKeys
	, "worked around grub/lvm boot bug #743126" ==>
		"/etc/default/grub" `File.containsLine` "GRUB_DISABLE_LINUX_UUID=true"
		`onChange` cmdProperty "update-grub" []
		`onChange` cmdProperty "update-initramfs" [Param "-u"]
	, "nuked cloudatcost cruft" ==> combineProperties
		[ File.notPresent "/etc/rc.local"
		, File.notPresent "/etc/init.d/S97-setup.sh"
		, User.nuked "user" User.YesReallyDeleteHome
		]
	]
