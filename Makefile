# Makefile


.PHONY: all
all: conlecterm run

conlecterm: $(wildcard *.hs)
	ghc -o "$@" --make Main.hs

.PHONY: run
run: conlecterm
	./conlecterm --config=config.rc


.PHONY: clean
clean:
	rm -f conlecterm *.o *.hi
