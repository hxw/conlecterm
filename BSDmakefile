# BSDmakefile

.PHONY: all
all:
	cabal new-build

INSTALL_DIR ?= ${HOME}/.local/share


.PHONY: install
install:
	cabal new-install conlecterm
	install conlecterm.desktop ${INSTALL_DIR}/applications
	install conlecterm.svg     ${INSTALL_DIR}/icons/Adwaita/scalable/apps



.PHONY: emacs-1
emacs-1:
	cabal new-run conlecterm -- -c . -v

.PHONY: emacs-2
emacs-2:
	cabal new-run conlecterm -- -c .
