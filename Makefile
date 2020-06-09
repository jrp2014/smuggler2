# For convenience
#

.PHONY: build install test clean accept doc hlint ghcid weed

all: build test doc

build:
	# Creates a package environment file needed to get the tests to run in some
	# environments (eg, travis)
	cabal build # --write-ghc-environment-files=always

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
	cabal v2-haddock --haddock-for-hackage --enable-doc
#	cabal upload -d --publish dist-newstyle/smuggler2-0.3.x.y-docs.tar.gz

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
