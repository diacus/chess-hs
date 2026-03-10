export LANG = en_US.UTF-8
export LC_ALL = en_US.UTF-8

run:
	cabal new-run chess

test:
	cabal new-run test

shell:
	cabal new-repl test

clean:
	$(RM) -r dist-newstyle

.PHONY: run test shell clean
