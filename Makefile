# For convenience
#

.PHONY: test clean accept doc hlint ghcid weed

all: test doc

test:
	 cabal test --test-show-details=streaming --test-option=--delete-output=onpass 

clean:
	cabal clean
	cabal v1-clean

accept:
	cabal run smuggler2-test -- --accept

doc:
	cabal haddock

hlint:
	hlint src test/Test.hs

ghcid:
	ghcid

weed:
	cabal build
	weeder
