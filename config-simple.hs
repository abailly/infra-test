-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import           Propellor
import           Propellor.CmdLine
-- import Propellor.Property.Scheduled
import           System.Posix.Files
import           Utility.FileMode

import           Capital.Property.Docker
import           Capital.Property.Firewall (firewallHttpsDockerSsh,
                                            openDevHttpPorts)
import           Capital.Property.Locale
import qualified Propellor.Property.Apache as Apache
import qualified Propellor.Property.Apt    as Apt
import qualified Propellor.Property.Cabal  as Cabal
import qualified Propellor.Property.Cron   as Cron
import qualified Propellor.Property.Docker as Docker
import           Propellor.Property.Fig    as Fig
import qualified Propellor.Property.File   as File
import qualified Propellor.Property.Git    as Git
import qualified Propellor.Property.Group  as Group
import qualified Propellor.Property.Ssh    as Ssh
import qualified Propellor.Property.Sudo   as Sudo
import qualified Propellor.Property.User   as User

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
-- Edit this to configure propellor!
hosts :: [Host]
hosts =
	[ host "beta.capital-match.com"
          & Git.installed
          & installLatestDocker
          & Fig.installed

          -- configure user build
          & User.accountFor "build"
          & User.hasGroup "build" "docker"
          & Ssh.keyImported SshRsa "build" (Context "beta.capital-match.com")
          & File.containsLines  "/home/build/.ssh/config"
          [ "Host bitbucket.org"
          , "\tUser git"
          , "\tHostname bitbucket.org"
          , "\tPreferredAuthentications publickey"
          , "\tIdentityFile \"/home/build/.ssh/id_rsa\""
				  ]
          & Ssh.knownExternalHost "bitbucket.org" "build"
          & Ssh.authorizedKeys "build" (Context "beta.capital-match.com")
          & Sudo.binaryEnabledFor "/usr/bin/docker" "build"
          -- configure ci. don't clone non-existant repository. It will delete the one present and there will be no repository at all.
          -- & Git.clonedBare "build" "git@bitbucket.org:capitalmatch/ci.git" "/home/build/ci.git"
          & File.hasContent "/home/build/ci.git/hooks/post-receive"
          [ "#!/bin/sh"
          , "read START STOP BRANCH"
          , "echo \"branch: $BRANCH\""
          , "expr \"$BRANCH\" : '.*/master' || exit 0"
          , "WORK_DIR=/home/build/ci/"
          , "GIT_WORK_TREE=${WORK_DIR} git checkout -f"
          , "cd $WORK_DIR"
          , "fig stop"
          , "fig build"
          , "fig up -d"
          ]
          & File.ownerGroup "/home/build/ci.git/hooks/post-receive" "build" "build"
          & "/home/build/ci.git/hooks/post-receive" `File.mode` combineModes [ownerWriteMode, ownerReadMode, ownerExecuteMode, groupReadMode, groupExecuteMode]
          & File.dirExists "/home/build/ci"
          & File.ownerGroup "/home/build/ci" "build" "build"
          & File.dirExists "/home/build/.ci"
          & File.ownerGroup "/home/build/.ci" "build" "build"
          -- configure app
          & dockerAuthTokenFor "build"
---          & Git.clonedBare "build" "git@bitbucket.org:capitalmatch/app.git" "/home/build/capital-match"
          & File.hasContent "/home/build/capital-match/hooks/post-receive"
          ["#!/bin/sh","#set -x","#set -e","read START STOP BRANCH","echo \"branch: $BRANCH\"","# do not send non review branches to CI","if expr \"$BRANCH\" : '.*/review' ; then ","  # assume docker is in path, we have right to use it and CI container is built","  docker run ci_server addpatch --name=$STOP --host=beta.capital-match.com","elif expr \"$BRANCH\" : '.*/master' ; then ","  if [ -f /home/build/.app.cid ]; then","    docker kill $(cat /home/build/.app.cid)","    rm /home/build/.app.cid","  fi","  # run as build user","  docker run -d --cidfile=/home/build/.app.cid -p 8080:8080 -v /home/build/data:/data capitalmatch/app:latest","  if [ -f /home/build/.nginx.cid ]; then","    docker kill $(cat /home/build/.nginx.cid)","    rm /home/build/.nginx.cid","  fi","  # clone or pull nginx config as build user","  export NGINXCONF=/home/build/nginxconf/nginx","  if [ -d /home/build/nginxconf ]; then","    cd /home/build/nginxconf && git pull origin master","  else ","    cd /home/build && git clone capital-match nginxconf","  fi","  docker run -d --cidfile=/home/build/.nginx.cid -p 80:80 -p 443:443 -v $NGINXCONF/nginx.conf:/etc/nginx/nginx.conf -v $NGINXCONF/sites-enabled:/etc/nginx/sites-enabled -v $NGINXCONF/certs:/etc/nginx/certs -v $NGINXCONF/logs:/var/log/nginx capital/nginx","fi","if [ -x ./hooks/git-slack-hook ]; then","    echo \"$START $STOP $BRANCH\" | ./hooks/git-slack-hook", "fi"]
          & File.mode "/home/build/capital-match/hooks/post-receive" (combineModes  (ownerWriteMode:readModes ++ executeModes))
          & File.dirExists "/home/build/bin"
          & File.containsLine "/home/build/.bash_profile" "PATH=/home/build/bin:$PATH"
          & File.hasPubContent "beta/docker-rm-stopped-containers-and-images.sh" "/home/build/bin/docker-rm-stopped-containers-and-images.sh"
          & Cron.niceJob "removing old docker images and containers" Cron.Weekly "build" "/home/build/bin" "/home/build/bin/docker-rm-stopped-containers-and-images.sh"


        , host "dev.capital-match.com"
          & installGhc783
          & devhost
          & installJava
          & installLein

        , host "angel"
          & installGhc783
          & devhost
          -- & installJava
          -- & installLein

        , host "test.atdd.io"
          & Docker.installed
          & setDefaultLocale en_us_UTF_8
          & Git.installed
          & User.accountFor "admin"
          & Sudo.binaryEnabledFor "/usr/bin/docker" "admin"
          & Ssh.authorizedKeys "admin" (Context "test.atdd.io")
          -- keys for pulling from private bitbucket repo
          & Ssh.keyImported SshRsa "admin" (Context "test.atdd.io")
          & File.containsLines "/home/admin/.ssh/config"
          [ "Host bitbucket.org"
          , "\tUser git"
          , "\tHostname bitbucket.org"
          , "\tPreferredAuthentications publickey"
          , "\tIdentityFile \"/home/admin/.ssh/id_rsa\""
          ]
          & Ssh.knownExternalHost "bitbucket.org" "admin"
          -- FIG
          & Fig.installed
          & firewallHttpsDockerSsh

          -- TODO Change hosting -> DO
        , host "92.243.3.60"
          & Git.installed
          & setDefaultLocale en_us_UTF_8
          & Apt.serviceInstalledRunning "apache2"
          & Apache.modEnabled "ssl"
          & User.accountFor "admin"
          & standardHakyllSite "admin" "admin" "atdd.io" ["www.atdd.io"]
          & standardHakyllSite "admin" "admin" "bailly.me" []
          & standardHakyllSite "admin" "admin" "blog.foldlabs.com" []
          & Git.bareRepo ("/home/admin/work" </> "cm.igitur.io.git") "admin" Git.NotShared
          & standardHakyllSite "admin" "admin" "cm.igitur.io" []
          & User.accountFor "www2"
          & User.hasGroup "www2" "admin"
          & Ssh.authorizedKeys "www2" (Context "www2.capital-match.com")
          & Git.bareRepo ("/home/www2/work" </> "www2.capital-match.com.git") "www2" Git.NotShared
          & standardHakyllSite "www2" "admin" "www2.capital-match.com" []

        , host "gypsyfire"
          & devhost
          & installEmacs4Haskell "willem"
          & configureEmacs "willem"
          & Cabal.installed "willem" ["stylish-haskell","hasktags"]

        -- new systemsthinking.net
        , host "advandenende.eu"
          & Apt.serviceInstalledRunning "apache2"
          & Apt.serviceInstalledRunning "mariadb"
          & User.accountFor "admin"
          & Apt.installed ["php5","libapache2-mod-php5","php5-mysql","locales"]
          & installLatestDocker
          & setDefaultLocale en_us_UTF_8
          & Git.installed
          & User.accountFor "admin"
          & Sudo.binaryEnabledFor "/usr/bin/docker" "admin"
          & firewallHttpsDockerSsh

	]

devhost :: Property HasInfo
devhost = propertyList "creating devserver configuration" $ props
          -- ipv4 takes precedence over ipv6 on ipv6 enabled host
          -- https://www.digitalocean.com/community/questions/how-to-disable-ubuntu-14-04-ipv6
          & File.containsLine "/etc/gai.conf" "precedence ::ffff:0:0/96 100"
          & setDefaultLocale en_us_UTF_8
          & Git.installed
          -- & installLatestDocker
          & Fig.installed
          -- configure user build
          & accountWithIds "build" 2020 2020
          & User.hasGroup "build" "docker"
          & Ssh.keyImported SshRsa "build" (Context "dev")
          & File.containsLines "/home/build/.ssh/config"
          [ "Host bitbucket.org"
          , "\tUser git"
          , "\tHostname bitbucket.org"
          , "\tPreferredAuthentications publickey"
          , "\tIdentityFile \"/home/build/.ssh/id_rsa\""
          ]
           & File.containsLines "/home/build/.ssh/config"
          [ "Host beta.capital-match.com"
          , "\tUser build"
          , "\tHostname beta.capital-match.com"
          , "\tPreferredAuthentications publickey"
          , "\tIdentityFile \"/home/build/.ssh/id_rsa\""
          ]
          & File.containsLine "/home/build/.bash_profile" "alias g=git"
          & Git.configuredUser "build" "Igitur Ventures Ltd." "igitur@igitur.io"
          & Ssh.knownExternalHost "bitbucket.org" "build"
          & Ssh.knownExternalHost "beta.capital-match.com" "build"
          & Ssh.authorizedKeys "build" (Context "dev")
          & Git.cloned "build" "ssh://build@beta.capital-match.com/~/capital-match" "/home/build/app" (Just "master")
          & File.hasPubContent "dev/app-git-config" "/home/build/app/.git/config"
          & installEmacs4Haskell "build"
          -- & configureEmacs "build"

          -- configure docker authent to pull images from dockerhub
          & dockerAuthTokenFor "build"
          & openDevHttpPorts

-- | Configures a hakyll-generated site as a vhost served by apache
standardHakyllSite :: UserName -> GroupName -> HostName -> [ HostName ] -> Property NoInfo
standardHakyllSite usr grp siteName aliases =
  propertyList ("serving " ++ siteName ++ " site")
  [ File.dirExists parent
  , File.dirExists directory
  , File.ownerGroup parent usr grp
  , File.ownerGroup directory usr grp
  , directory `File.mode` combineModes [ownerWriteMode, ownerReadMode, ownerExecuteMode, groupReadMode, groupExecuteMode]
  , ignoreInfo $ toProp $ Apache.siteEnabled siteName $ apachecfg siteName aliases directory NoSSL []
  ]
  where
    parent = "/srv/nono-data" </> siteName
    directory = parent </> "_site"

-- this is ok-ish, same as Dockerfiles
installGhc783 :: Property HasInfo
installGhc783 = propertyList "installing ghc-7.8.3 from apt" $ props
                & scriptProperty ["add-apt-repository -y ppa:hvr/ghc "]
                & Apt.update
                & Apt.installed [ "build-essential", "ghc-7.8.3", "cabal-install-1.20", "alex", "happy" ]
                & File.containsLine "/home/build/.bash_profile" "export PATH=/home/build/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:$PATH"


-- this is crude
installLein :: Property NoInfo
installLein = scriptProperty [ "wget -O /usr/local/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein"
                             , "chmod 0755 /usr/local/bin/lein"
                             ]

-- not speaking of this !
installJava :: Property NoInfo
installJava = scriptProperty [ "DEBIAN_FRONTEND=noninteractive apt-get install -y software-properties-common python-software-properties"
                             ,  "add-apt-repository -y ppa:webupd8team/java"
                             ,  "apt-get update -q"
                             ,  "echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections"
                             ,  "echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections"
                             , "DEBIAN_FRONTEND=noninteractive apt-get install -y oracle-java8-installer"
                             , "apt-get clean"
                             ]

data VHostSSL = NoSSL
              | WithSSL
              deriving (Eq,Show,Read)

-- | Configuration for apache virtual host
-- stolen from JoeysSites
apachecfg :: HostName             -- ^Host's name
          -> [HostName]             -- ^Host's name
          -> FilePath            -- ^Path to document root
          -> VHostSSL            -- ^Configure SSL virtual host?
          -> Apache.ConfigFile   -- ^Configuration file to modify
          -> Apache.ConfigFile
apachecfg hn aliases documentRoot withSSL middle
  | withSSL == WithSSL = vhost NoSSL  ++ vhost WithSSL
  | otherwise         = vhost NoSSL
  where
	vhost ssl =
          [ "<VirtualHost *:"++show port++">"
          , "  ServerAdmin arnaud@foldlabs.com"
          , "  ServerName "++hn++":"++show port
          ]
          ++ map serverAlias aliases ++
          [ "  DocumentRoot " ++ documentRoot
          , "  <Directory " ++ documentRoot ++ ">"
          , "    Options Indexes FollowSymlinks MultiViews"
          , "    AllowOverride None"
          , "    Order allow,deny"
          , "    Allow from all"
          , "  </Directory>"
          , "  ScriptAlias /cgi-bin/ /usr/lib/cgi-bin/"
          , "  <Directory /usr/lib/cgi-bin>"
          , "    SetHandler cgi-script"
          , "    Options +ExecCGI -MultiViews +SymLinksIfOwnerMatch"
          , "  </Directory>"

          ]
          ++ mainhttpscert ssl
          ++ middle ++
          [ ""
          , " ErrorLog ${APACHE_LOG_DIR}/"++ hn ++ ".error.log"
          , " LogLevel warn"
          , " CustomLog ${APACHE_LOG_DIR}/"++hn++".access.log combined"
          , "  ServerSignature On"
          , "  "
          , "  <Directory \"/usr/share/apache2/icons\">"
          , "      Options Indexes MultiViews"
          , "      AllowOverride None"
          , "      Order allow,deny"
          , "      Allow from all"
          , "  </Directory>"
          , "</VirtualHost>"
          ]
          where
            port = case withSSL of
                    NoSSL   -> 80 :: Int
                    WithSSL -> 443 :: Int
            serverAlias hostname = "  ServerAlias " ++ hostname

mainhttpscert :: VHostSSL -> Apache.ConfigFile
mainhttpscert NoSSL   = []
mainhttpscert WithSSL =
  [ "  SSLEngine on"
  , "  SSLCertificateFile /etc/ssl/certs/web.pem"
  , "  SSLCertificateKeyFile /etc/ssl/private/web.pem"
  , "  SSLCertificateChainFile /etc/ssl/certs/startssl.pem"
  ]

accountWithIds :: UserName -> Int -> Int -> Property NoInfo
accountWithIds user uid gid = check (isNothing <$> catchMaybeIO (User.homedir user)) $ propertyList
                              ("account for " ++ user ++ " with uid:" ++ (show uid) ++ "/gid:" ++ (show gid))
                              [ Group.exists user (Just  gid)
                              , cmdProperty "adduser"
                                [ "--disabled-password"
                                , "--gecos", ""
                                , "--uid", (show uid)
                                , "--gid", (show gid)
                                , user
                                ]
                              ]

installEmacs4Haskell :: UserName -> Property NoInfo
installEmacs4Haskell user = property ("installing emacs and cabal packages for haskell development for user " ++ user) $ do
  ensureProperty $ combineProperties "installing emacs and supporting haskell packages"
    [ Cabal.updated user
    , Apt.installed [ "emacs24", "zlib1g-dev" ]
    , File.containsLine (home </> ".bash_profile") ("export PATH=" <> home </> ".cabal/bin:$PATH")
    , Cabal.toolsInstalledInSandbox user ("/home" </> user </> "haskell-tools") ["shake"]
    , Cabal.toolsInstalledInSandbox user ("/home" </> user </> "emacs-tools") ["ghc-mod", "stylish-haskell" ]
    ]
  where
    home = "/home" </> user-- liftIO $ User.homedir (user) didn't compile, TODO fix.

configureEmacs :: UserName -> Property NoInfo
configureEmacs user = property ("configuring emacs for haskell development for user " ++ user) $ do
  home <- liftIO $ User.homedir user
  ensureProperty $ combineProperties "creating emacs configuration"
    [ File.dirExists (home </> ".emacs.d")
    , File.hasContent ("/root/.tmux.conf") [ "setw -g xterm-keys on" ]
    , File.ownerGroup (home </> ".emacs.d") user user
    , File.hasContent (home </> ".emacs.d/install-package.el")
      [ "(require 'package)"
      , "(package-initialize)"
      , "(add-to-list 'package-archives"
      , "             '(\"melpa\" . \"http://melpa.milkbox.net/packages/\") t)"
      , "(add-to-list 'package-archives"
      , "             '(\"marmalade\" . \"http://marmalade-repo.org/packages/\") t)"
      , ";; Fix HTTP1/1.1 problems"
      , "(setq url-http-attempt-keepalives nil)"
      , "(package-refresh-contents)"
      , "(mapc 'package-install pkg-to-install)"
      ]
    , userScriptProperty user [ "emacs --batch --eval \"(defconst pkg-to-install '(flycheck auto-complete haskell-mode ghc projectile flx-ido clojure-mode))\" -l $HOME/.emacs.d/install-package.el" ]
    , File.hasContent (home </> ".emacs")
      [ "(add-to-list 'exec-path \"~/.cabal/bin\")"
      , "(menu-bar-mode 0)"
      , ""
      , ";; package installation"
      , "(require 'package)"
      , "(add-to-list 'package-archives"
      , "             '(\"marmalade\" . \"http://marmalade-repo.org/packages/\"))"
      , "(add-to-list 'package-archives"
      , "             '(\"melpa\" . \"http://melpa.milkbox.net/packages/\"))"
      , ""
      , "(package-initialize)"
      , ""
      , ";; projectile"
      , ";; https://github.com/bbatsov/projectile"
      , "(require 'projectile)"
      , "(projectile-global-mode)"
      , "(setq projectile-indexing-method 'native)"
      , "(setq projectile-enable-caching t)"
      , ""
      , ";; flx"
      , "(require 'flx-ido)"
      , "(ido-mode 1)"
      , "(ido-everywhere 1)"
      , "(flx-ido-mode 1)"
      , ""
      , ";; disable ido faces to see flx highlights."
      , "(setq ido-use-faces nil)"
      , ""
      , ";; use space for indentation, 2 spaces wide"
      , "(setq-default indent-tabs-mode nil)"
      , "(setq-default tab-width 2)"
      , ""
      , ";; activate smerge when opening conflict files"
      , "(defun sm-try-smerge ()"
      , "     (save-excursion"
      , "       (goto-char (point-min))"
      , "       (when (re-search-forward \"^<<<<<<< \" nil t)"
      , "        (smerge-mode 1))))"
      , ""
      , "(add-hook 'find-file-hook 'sm-try-smerge t)"
      , ""
      , ""
      , ""
      , ";; haskell coding"
      , "(require 'auto-complete)"
      , "(require 'haskell-mode)"
      , "(require 'haskell-cabal)"
      , ""
      , "(autoload 'ghc-init \"ghc\" nil t)"
      , ""
      , "(add-hook 'haskell-mode-hook (lambda () (ghc-init)))"
      , ""
      , "(eval-after-load \"haskell-mode\""
      , "  '(progn"
      , "     (setq haskell-stylish-on-save t)"
      , "     (setq haskell-process-args-cabal-repl '(\"--ghc-option=-ferror-spans\""
      , "                                             \"--with-ghc=ghci-ng\"))     "
      , "     "
      , "     (define-key haskell-mode-map (kbd \"C-,\") 'haskell-move-nested-left)"
      , "     (define-key haskell-mode-map (kbd \"C-.\") 'haskell-move-nested-right)"
      , "     (define-key haskell-mode-map (kbd \"C-c v c\") 'haskell-cabal-visit-file)"
      , "     (define-key haskell-mode-map (kbd \"C-c C-c\") 'haskell-compile)"
      , "     (define-key haskell-mode-map (kbd \"C-x C-d\") nil)"
      , "     (setq haskell-font-lock-symbols t)"
      , ""
      , "     ;; Do this to get a variable in scope"
      , "     (auto-complete-mode)"
      , ""
      , "     ;; from http://pastebin.com/tJyyEBAS"
      , "     (ac-define-source ghc-mod"
      , "       '((depends ghc)"
      , "         (candidates . (ghc-select-completion-symbol))"
      , "         (symbol . \"s\")"
      , "         (cache)))"
      , "     "
      , "     (defun my-ac-haskell-mode ()"
      , "       (setq ac-sources '(ac-source-words-in-same-mode-buffers"
      , "                          ac-source-dictionary"
      , "                          ac-source-ghc-mod)))"
      , "     (add-hook 'haskell-mode-hook 'my-ac-haskell-mode)"
      , "     "
      , "  "
      , "     (defun my-haskell-ac-init ()"
      , "       (when (member (file-name-extension buffer-file-name) '(\"hs\" \"lhs\"))"
      , "         (auto-complete-mode t)"
      , "         (setq ac-sources '(ac-source-words-in-same-mode-buffers"
      , "                            ac-source-dictionary"
      , "                            ac-source-ghc-mod))))"
      , "     (add-hook 'find-file-hook 'my-haskell-ac-init)))"
      , ""
      , "(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)"
      , "(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)"
      , ""
      , "(eval-after-load \"which-func\""
      , "  '(add-to-list 'which-func-modes 'haskell-mode))"
      , ""
      , "(eval-after-load \"haskell-cabal\""
      , "    '(define-key haskell-cabal-mode-map (kbd \"C-c C-c\") 'haskell-compile))"
        -- handle tmux's xterm-keys
        -- from http://unix.stackexchange.com/questions/24414/shift-arrow-not-working-in-emacs-within-tmux
        --  put the following line in your ~/.tmux.conf:
        --   setw -g xterm-keys on
      , "(define-key input-decode-map \"\\e[1;5C\" [C-right])"
      , "(define-key input-decode-map \"\\e[1;5D\" [C-left])"
      , "(if (getenv \"TMUX\")"
      , "    (progn"
      , "      (let ((x 2) (tkey \"\"))"
      , "    (while (<= x 8)"
      , "      ;; shift"
      , "      (if (= x 2)"
      , "          (setq tkey \"S-\"))"
      , "      ;; alt"
      , "      (if (= x 3)"
      , "          (setq tkey \"M-\"))"
      , "      ;; alt + shift"
      , "      (if (= x 4)"
      , "          (setq tkey \"M-S-\"))"
      , "      ;; ctrl"
      , "      (if (= x 5)"
      , "          (setq tkey \"C-\"))"
      , "      ;; ctrl + shift"
      , "      (if (= x 6)"
      , "          (setq tkey \"C-S-\"))"
      , "      ;; ctrl + alt"
      , "      (if (= x 7)"
      , "          (setq tkey \"C-M-\"))"
      , "      ;; ctrl + alt + shift"
      , "      (if (= x 8)"
      , "          (setq tkey \"C-M-S-\"))"
      , "      ;; arrows"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d A\" x)) (kbd (format \"%s<up>\" tkey)))"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d B\" x)) (kbd (format \"%s<down>\" tkey)))"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d C\" x)) (kbd (format \"%s<right>\" tkey)))"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d D\" x)) (kbd (format \"%s<left>\" tkey)))"
      , "      ;; home"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d H\" x)) (kbd (format \"%s<home>\" tkey)))"
      , "      ;; end"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d F\" x)) (kbd (format \"%s<end>\" tkey)))"
      , "      ;; page up"
      , "      (define-key key-translation-map (kbd (format \"M-[ 5 ; %d ~\" x)) (kbd (format \"%s<prior>\" tkey)))"
      , "      ;; page down"
      , "      (define-key key-translation-map (kbd (format \"M-[ 6 ; %d ~\" x)) (kbd (format \"%s<next>\" tkey)))"
      , "      ;; insert"
      , "      (define-key key-translation-map (kbd (format \"M-[ 2 ; %d ~\" x)) (kbd (format \"%s<delete>\" tkey)))"
      , "      ;; delete"
      , "      (define-key key-translation-map (kbd (format \"M-[ 3 ; %d ~\" x)) (kbd (format \"%s<delete>\" tkey)))"
      , "      ;; f1"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d P\" x)) (kbd (format \"%s<f1>\" tkey)))"
      , "      ;; f2"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d Q\" x)) (kbd (format \"%s<f2>\" tkey)))"
      , "      ;; f3"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d R\" x)) (kbd (format \"%s<f3>\" tkey)))"
      , "      ;; f4"
      , "      (define-key key-translation-map (kbd (format \"M-[ 1 ; %d S\" x)) (kbd (format \"%s<f4>\" tkey)))"
      , "      ;; f5"
      , "      (define-key key-translation-map (kbd (format \"M-[ 15 ; %d ~\" x)) (kbd (format \"%s<f5>\" tkey)))"
      , "      ;; f6"
      , "      (define-key key-translation-map (kbd (format \"M-[ 17 ; %d ~\" x)) (kbd (format \"%s<f6>\" tkey)))"
      , "      ;; f7"
      , "      (define-key key-translation-map (kbd (format \"M-[ 18 ; %d ~\" x)) (kbd (format \"%s<f7>\" tkey)))"
      , "      ;; f8"
      , "      (define-key key-translation-map (kbd (format \"M-[ 19 ; %d ~\" x)) (kbd (format \"%s<f8>\" tkey)))"
      , "      ;; f9"
      , "      (define-key key-translation-map (kbd (format \"M-[ 20 ; %d ~\" x)) (kbd (format \"%s<f9>\" tkey)))"
      , "      ;; f10"
      , "      (define-key key-translation-map (kbd (format \"M-[ 21 ; %d ~\" x)) (kbd (format \"%s<f10>\" tkey)))"
      , "      ;; f11"
      , "      (define-key key-translation-map (kbd (format \"M-[ 23 ; %d ~\" x)) (kbd (format \"%s<f11>\" tkey)))"
      , "      ;; f12"
      , "      (define-key key-translation-map (kbd (format \"M-[ 24 ; %d ~\" x)) (kbd (format \"%s<f12>\" tkey)))"
      , "      ;; f13"
      , "      (define-key key-translation-map (kbd (format \"M-[ 25 ; %d ~\" x)) (kbd (format \"%s<f13>\" tkey)))"
      , "      ;; f14"
      , "      (define-key key-translation-map (kbd (format \"M-[ 26 ; %d ~\" x)) (kbd (format \"%s<f14>\" tkey)))"
      , "      ;; f15"
      , "      (define-key key-translation-map (kbd (format \"M-[ 28 ; %d ~\" x)) (kbd (format \"%s<f15>\" tkey)))"
      , "      ;; f16"
      , "      (define-key key-translation-map (kbd (format \"M-[ 29 ; %d ~\" x)) (kbd (format \"%s<f16>\" tkey)))"
      , "      ;; f17"
      , "      (define-key key-translation-map (kbd (format \"M-[ 31 ; %d ~\" x)) (kbd (format \"%s<f17>\" tkey)))"
      , "      ;; f18"
      , "      (define-key key-translation-map (kbd (format \"M-[ 32 ; %d ~\" x)) (kbd (format \"%s<f18>\" tkey)))"
      , "      ;; f19"
      , "      (define-key key-translation-map (kbd (format \"M-[ 33 ; %d ~\" x)) (kbd (format \"%s<f19>\" tkey)))"
      , "      ;; f20"
      , "      (define-key key-translation-map (kbd (format \"M-[ 34 ; %d ~\" x)) (kbd (format \"%s<f20>\" tkey)))"
      , "      (setq x (+ x 1))"
      , "      ))"
      , "    )"
      , "  ) "
      ]
    , File.ownerGroup (home </> ".emacs") user user
    ]
