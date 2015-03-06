module Capital.Property.Firewall (firewallHttpsDockerSsh, openDevHttpPorts) where

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

openDevHttpPorts :: Property HasInfo
openDevHttpPorts = propertyList "creating firewall rules for dev ports " $ props
        & Firewall.installed
        & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
        & Firewall.rule INPUT ACCEPT (IFace "lo")
        & Firewall.rule INPUT ACCEPT (IFace "docker0")
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 22)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 80)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 443)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 8085)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 8095)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 8080)
        & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 8081)

        & Firewall.rule INPUT DROP   Everything
