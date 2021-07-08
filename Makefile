run:
	cabal new-run chess

test:
	cabal new-run test

shell:
	cabal new-repl test

.PHONY: run test shell
