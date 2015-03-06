module Capital.Property.Firewall (firewallHttpsDockerSsh, openDevHttpPorts) where

import           Propellor
import           Propellor.Property.Firewall as Firewall

firewallPreamble :: Property HasInfo
firewallPreamble = propertyList "standard firewall preamble rules (opens lo and docker0)" $ props
                   & Firewall.installed
                   & Firewall.rule INPUT ACCEPT (Ctstate [ESTABLISHED,RELATED])
                   & Firewall.rule INPUT ACCEPT (IFace "lo")
                   & Firewall.rule INPUT ACCEPT (IFace "docker0")

openCommonPorts :: Property HasInfo
openCommonPorts =  propertyList "open common operating ports for web" $ props
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 22)
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 80)
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- Port 443)

firewallHttpsDockerSsh :: Property HasInfo
firewallHttpsDockerSsh = propertyList "firewall accepts ssh, http(s) and docker" $ props
                         & firewallPreamble
                         & openCommonPorts
                         & dropEverything

openDevHttpPorts :: Property HasInfo
openDevHttpPorts = propertyList "firewall accepts standard ports and a dev range for ports" $ props
                   & firewallPreamble
                   & openCommonPorts
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- PortRange (8080,8099))
                   & Firewall.rule INPUT ACCEPT (Proto TCP :- PortRange (5900,5903))
                   & dropEverything
