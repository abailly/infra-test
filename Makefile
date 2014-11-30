CABAL?=cabal

DEBDEPS=gnupg ghc cabal-install libghc-missingh-dev libghc-ansi-terminal-dev libghc-ifelse-dev libghc-unix-compat-dev libghc-hslogger-dev libghc-network-dev libghc-quickcheck2-dev libghc-mtl-dev libghc-monadcatchio-transformers-dev

# this target is provided to keep old versions of the propellor cron job
# working, and will eventually be removed
run: deps build
	./propellor

dev: build tags

build: dist/setup-config
	@if ! $(CABAL) build; then $(CABAL) configure; $(CABAL) build; fi
	@ln -sf dist/build/propellor-config/propellor-config propellor

deps:
	@if [ $$(whoami) = root ]; then apt-get --no-upgrade --no-install-recommends -y install $(DEBDEPS) || (apt-get update && apt-get --no-upgrade --no-install-recommends -y install $(DEBDEPS)); fi || true
	@if [ $$(whoami) = root ]; then apt-get --no-upgrade --no-install-recommends -y install libghc-async-dev || (cabal update; cabal install async); fi || true

dist/setup-config: propellor.cabal
	@if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	@$(CABAL) configure

install: propellor.1
	install -d $(DESTDIR)/usr/bin $(DESTDIR)/usr/src/propellor
	install -s dist/build/propellor/propellor $(DESTDIR)/usr/bin/propellor
	mkdir -p dist/gittmp
	$(CABAL) sdist
	cat dist/propellor-*.tar.gz | (cd dist/gittmp && tar zx --strip-components=1)
	# cabal sdist does not preserve symlinks, so copy over file
	cd dist/gittmp && for f in $$(find -type f); do rm -f $$f; cp -a ../../$$f $$f; done
	cd dist/gittmp && git init && \
		git add . \
		&& git commit -q -m "distributed version of propellor" \
		&& git bundle create $(DESTDIR)/usr/src/propellor/propellor.git master HEAD \
		&& git show-ref master --hash > $(DESTDIR)/usr/src/propellor/head
	rm -rf dist/gittmp

propellor.1: doc/usage.mdwn doc/mdwn2man
	doc/mdwn2man propellor 1 < doc/usage.mdwn > propellor.1

clean:
	rm -rf dist Setup tags propellor propellor.1 privdata/local
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# hothasktags chokes on some template haskell etc, so ignore errors
# duplicate tags with Propellor.Property. removed from the start, as we
# often import qualified by just the module base name.
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags | perl -ne 'print; s/Propellor\.Property\.//; print' | sort > tags  2>/dev/null

# Upload to hackage.
hackage:
	@cabal sdist
	@cabal upload dist/*.tar.gz

.PHONY: tags
