.PHONY: build clean repl watch ;\
	cic ci formatc format lint lintc haddockc ;\
	haddock hackage

# core

T = ""

build:
	if [ -z "$(T)" ]; then \
		cabal build all; \
	else \
		cabal build $(T); \
	fi

clean:
	cabal clean

repl:
	cabal repl $(T)

watch:
	ghcid --command "cabal repl $(T)"

# ci

cic: formatc lintc haddockc

ci: lint format

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode check ;\
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt -- --check

format:
	nix run github:tbidne/nix-hs-tools/0.7#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.7#ormolu -- --mode inplace ;\
	nix run github:tbidne/nix-hs-tools/0.7#nixpkgs-fmt

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.7#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.7#hlint

# generate docs for main package, copy to docs/
haddock:
	cabal haddock all --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	rm -rf docs/effectul-callstack* ;\
	rm -rf docs/effectul-fs* ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/effectful-callstack-0.1/doc/html/* docs/ ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.5/effectful-fs-0.1/doc/html/* docs/

haddockc:
	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- \
		./effectful-callstack \

	nix run github:tbidne/nix-hs-tools/0.7#haddock-cov -- \
		./effectful-fs \

.PHONY: hackage
hackage:
	cabal sdist ;\
	cabal haddock --haddock-for-hackage --enable-doc
