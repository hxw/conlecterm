# BSDmakefile

.PHONY: all
all:
	cabal new-update
	cabal new-build

INSTALL_DIR ?= ${HOME}/.local/share
BIN_DIR ?= ${HOME}/.cabal/bin

THEMES += Adwaita
THEMES += hicolor

.PHONY: run
run:
	cabal new-run

.PHONY: install
install:
	rm -f "${BIN_DIR}/conlecterm"
	cabal new-install conlecterm
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

.PHONY: emacs-1
emacs-1:
	cabal new-run conlecterm -- -c . -v

.PHONY: emacs-2
emacs-2:
	cabal new-run conlecterm -- -c .
