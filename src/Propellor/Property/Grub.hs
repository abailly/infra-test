module Propellor.Property.Grub where

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

-- | Eg, \"hd0,0\" or \"xen/xvda1\"
type GrubDevice = String

-- | Eg, \"\/dev/sda\"
type OSDevice = String

type TimeoutSecs = Int

-- | Types of machines that grub can boot.
data BIOS = PC | EFI64 | EFI32 | Coreboot | Xen

-- | Installs the grub package. This does not make grub be used as the
-- bootloader.
--
-- This includes running update-grub, so that the grub boot menu is
-- created. It will be automatically updated when kernel packages are
-- installed.
installed :: BIOS -> Property NoInfo
installed bios = 
	Apt.installed [pkg] `describe` "grub package installed"
		`before`
	cmdProperty "update-grub" []
  where
	pkg = case bios of
		PC -> "grub-pc"
		EFI64 -> "grub-efi-amd64"
		EFI32 -> "grub-efi-ia32"
		Coreboot -> "grub-coreboot"
		Xen -> "grub-xen"

-- | Installs grub onto a device, so the system can boot from that device.
--
-- You may want to install grub to multiple devices; eg for a system
-- that uses software RAID.
--
-- Note that this property does not check if grub is already installed
-- on the device; it always does the work to reinstall it. It's a good idea
-- to arrange for this property to only run once, by eg making it be run
-- onChange after OS.cleanInstallOnce.
boots :: OSDevice -> Property NoInfo
boots dev = cmdProperty "grub-install" [dev]
	`describe` ("grub boots " ++ dev)

-- | Use PV-grub chaining to boot
--
-- Useful when the VPS's pv-grub is too old to boot a modern kernel image.
--
-- <http://notes.pault.ag/linode-pv-grub-chainning/>
--
-- The rootdev should be in the form "hd0", while the bootdev is in the form
-- "xen/xvda".
chainPVGrub :: GrubDevice -> GrubDevice -> TimeoutSecs -> Property NoInfo
chainPVGrub rootdev bootdev timeout = combineProperties desc
	[ File.dirExists "/boot/grub"
	, "/boot/grub/menu.lst" `File.hasContent`
		[ "default 1" 
		, "timeout " ++ show timeout
		, ""
		, "title grub-xen shim"
		, "root (" ++ rootdev ++ ")"
		, "kernel /boot/xen-shim"
		, "boot"
		]
	, "/boot/load.cf" `File.hasContent`
		[ "configfile (" ++ bootdev ++ ")/boot/grub/grub.cfg" ]
	, installed Xen
	, flagFile (scriptProperty ["grub-mkimage --prefix '(" ++ bootdev ++ ")/boot/grub' -c /boot/load.cf -O x86_64-xen /usr/lib/grub/x86_64-xen/*.mod > /boot/xen-shim"]) "/boot/xen-shim"
			`describe` "/boot-xen-shim"
	]
  where
	desc = "chain PV-grub"
