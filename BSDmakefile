# BSDmakefile

.PHONY: all
all:
	cabal update
	cabal build

INSTALL_DIR ?= ${HOME}/.local/share
BIN_DIR ?= ${HOME}/.cabal/bin

THEMES += Adwaita
THEMES += hicolor

.PHONY: run
run:
	cabal run

.PHONY: install
install:
	rm -f "${BIN_DIR}/conlecterm"
	cabal install conlecterm
	install -d "${INSTALL_DIR}/applications"
	install conlecterm.desktop ${INSTALL_DIR}/applications
.for t in ${THEMES}
	install -d "${INSTALL_DIR}/icons/${t}/scalable/apps"
	install conlecterm.svg "${INSTALL_DIR}/icons/${t}/scalable/apps"
.endfor

.PHONY: clean
clean:
	rm -fr dist dist-newstyle

# information for updating the FreeBSD port x11/conlecterm
.PHONY: tuple
tuple:
	cabal2tuple dist-newstyle/cache/plan.json

.PHONY: test-1
test-1:
	cabal run conlecterm -- -c . -v

.PHONY: test-2
test-2:
	cabal run conlecterm -- -c .
