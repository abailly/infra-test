module Capital.Property.Firewall (firewallHttpsDockerSsh) where

import           Propellor
import           Propellor.Property.Firewall as Firewall

firewallHttpsDockerSsh :: Property HasInfo
firewallHttpsDockerSsh = propertyList "creating firewall for ssh, http(s) and docker" $ props
        & Firewall.installed
        & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
        & Firewall.rule INPUT ACCEPT (IFace "lo")
        & Firewall.rule INPUT ACCEPT (IFace "docker0")
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 22)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 80)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 443)
        & Firewall.rule INPUT DROP   Everything
