# For convenience
#

.PHONY: build install test clean accept doc hlint ghcid upload upload-docs weed

all: build test doc

build:
	# Creates a package environment file needed to get the tests to run in some
	# environments (eg, travis).  Use with care as it can lead to unexpected
	# results if you are not aware that ghc is using it; it is a normally hidden
	# dot file.
	cabal outdated
	cabal build all:libs
	cabal build all:exes
	cabal build all:tests

	#cabal build # --write-ghc-environment-files=always

debug:
	cabal build -fdebug

install:
	cabal install --lib smuggler2
	cabal install exe:ghc-smuggler2 --overwrite-policy=always

test: build
	git diff --check
	cabal test --test-show-details=streaming --test-option=--delete-output=onpass

clean:
	cabal clean
	cabal v1-clean

accept:
	cabal run smuggler2-test -- --accept --delete-output=onpass

doc:
	cabal haddock
	cabal haddock --haddock-for-hackage --enable-doc --haddock-option=--hyperlinked-source

upload:
	cabal check
	cabal gen-bounds
	cabal sdist
	cabal upload dist-newstyle/sdist/smuggler2-0.3.*.*.tar.gz

upload-docs:
	cabal haddock --haddock-for-hackage --enable-doc --haddock-option=--hyperlinked-source
	cabal upload -d --publish dist-newstyle/smuggler2-0.3.*.*-docs.tar.gz

hlint:
	hlint src test/Test.hs

ghcid:
	ghcid

weed:
	cabal build
	weeder

whitespace:
	 git diff --check
	 git rebase --whitespace=fix
