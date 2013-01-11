# Makefile

DESTDIR ?= ${HOME}/bin

.PHONY: all
all: conlecterm run

.PHONY: install
install: conlecterm
	install conlecterm "${DESTDIR}"


conlecterm: $(wildcard *.hs)
	ghc -o "$@" --make Main.hs
#	ghc -o "$@" -threaded --make Main.hs

.PHONY: run
run: conlecterm
	./conlecterm --config=config.rc


.PHONY: clean
clean:
	rm -f conlecterm *.o *.hi
