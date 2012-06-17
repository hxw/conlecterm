# Makefile


.PHONY: all
all: conlecterm

conlecterm: Main.hs ConfigurationParser.hs TerminalUI.hs ProcessRunner.hs
	ghc -o "$@" --make Main.hs

.PHONY: run
run: conlecterm
	./conlecterm --config=config.rc


.PHONY: clean
clean:
	rm -f conlecterm *.o *.hi
