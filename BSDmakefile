# BSDmakefile

.PHONY: all
all:
	cabal new-build

INSTALL_DIR ?= ${HOME}/.local/share
BIN_DIR ?= ${HOME}/.cabal/bin

THESES += Adwaita
THEMES += hicolor

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


.PHONY: emacs-1
emacs-1:
	cabal new-run conlecterm -- -c . -v

.PHONY: emacs-2
emacs-2:
	cabal new-run conlecterm -- -c .
