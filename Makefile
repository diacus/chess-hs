run:
	cabal run chess

test:
	cabal run test

shell:
	cabal new-repl chess

.PHONY: run test shell
