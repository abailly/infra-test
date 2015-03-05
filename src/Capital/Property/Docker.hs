-- | To install latest docker from docker.com
-- Ubuntu is quite far behind, so we don't want to use Propellors' default docker support which will
-- replace a recent docker with ubuntu's one.
module Capital.Property.Docker (installLatestDocker) where
import qualified Propellor.Property.Apt  as Apt
import qualified Propellor.Property.File as File


installLatestDocker :: Property NoInfo
installLatestDocker = propertyList ("install latest docker from official repositories")
					  [
  cmdProperty "apt-key" [ "adv"
						, "--keyserver"
						, "hkp://keyserver.ubuntu.com:80"
						, "--recv-keys"
						, "36A1D7869245C8950F966E92D8576A8BA88D21E9"
						]
  , File.containsLines "/etc/apt/sources.list.d/docker.list"
	 ["deb https://get.docker.com/ubuntu docker main"]
  , Apt.update
  , Apt.installed [ "lxc-docker" ]
  ]

