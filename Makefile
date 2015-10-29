# Makefile

DESTDIR ?= ${HOME}/bin

SOURCES += ConfigurationParser.hs
SOURCES += Main.hs
SOURCES += ProcessRunner.hs
SOURCES += SendControl.hs
SOURCES += TerminalUI.hs

.PHONY: all
#all: conlecterm run
all:
	cabal build

.PHONY: deps
deps:
	cabal install --dependencies-only

.PHONY: install
install: conlecterm
	install conlecterm "${DESTDIR}"

SOURCES = Main.hs
SOURCES += ConfigurationParser.hs
SOURCES += ProcessRunner.hs
SOURCES += SendControl.hs
SOURCES += TerminalUI.hs

conlecterm: ${SOURCES}
	ghc -o "$@" --make Main.hs
#	ghc -o "$@" -threaded --make Main.hs

.PHONY: run
run: conlecterm
	./conlecterm --verbose --config=.


.PHONY: clean
clean:
	rm -f conlecterm *.o *.hi session-*.rc
