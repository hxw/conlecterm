# BSDmakefile

# basic settings
PROGRAM ?= SetMe

# need to include gtk2hs
USE_GTK ?= NO

# a default agument list for run - averride as required
PROG_ARGS ?= -v -c ${PROGRAM}.conf

# default location of cabal built binary
PROG_BIN ?= ./dist/build/${PROGRAM}/${PROGRAM}

# desktop file
HAS_DESKTOP_FILE ?= NO
PROG_DESKTOP ?= ${PROGRAM}.desktop

# installation directory
INSTALL_DIR ?= ${HOME}/bin
APPLICATIONS_DIR ?= ${HOME}/.local/share/applications

# the default cabal file
CABAL_FILE ?= ${PROGRAM}.cabal
CABAL_SANDBOX = .cabal-sandbox

# utilities
RM = rm -f
SAY = echo
SED = sed
INSTALL_PROGRAM ?= install -C
INSTALL_CONFIG  ?= cp -p

# default target
.PHONY: all
all: setup deps
	cabal build

.PHONY: help
help:
	@${SAY} 'list of make targets for application: "${PROGRAM}"'
	@${SAY} '  all             - build the application'
	@${SAY} '  deps            - update sandbox if "${CABAL_FILE}" was changed'
	@${SAY} '  run             - run the application (build if necessary)'
	@${SAY} '  install         - install the application binary in "${INSTALL_DIR}" (build if necessary)'
	@${SAY} '  setup           - install the cabal-sandbox if not already installed'
	@${SAY} '  clean           - clean up application objec/binary files'
	@${SAY} '  complete-clean  - clean then destroy sandbox'


# run the program with supplied arguments
.PHONY: run
run: all
	${PROG_BIN} ${PROG_ARGS}

# install any new dependencies
# run this if  the .cabal file changes
.PHONY: deps
deps: ${CABAL_FILE}
	cabal install --only-dependencies

# install the application binary
.PHONY: install pre-install do-install post-install
install: pre-install do-install post-install
pre-install:
do-install:
	@if [ ! -d '${INSTALL_DIR}' ]; then ${SAY} 'missing directory: ${INSTALL_DIR}'; false; fi
	${INSTALL_PROGRAM} '${PROG_BIN}' '${INSTALL_DIR}/${PROGRAM}'
.if "YES" == "${HAS_DESKTOP_FILE}"
	@if [ ! -d '${APPLICATIONS_DIR}' ]; then ${SAY} 'missing  directory: ${APPLICATIONS_DIR}'; false; fi
	${SED} 's,@BINDIR@,${INSTALL_DIR}/,' < '${PROG_DESKTOP}' > '${APPLICATIONS_DIR}/${PROG_DESKTOP}'
.endif
post-install:

# install sandbox if not already installed and build
.PHONY: sandbox-init
sandbox-init:
	@[ -d .cabal-sandbox ] || ${MAKE} setup

# setup sandbox and install required tools
.PHONY: setup
setup: ${CABAL_SANDBOX}

${CABAL_SANDBOX}:
	cabal sandbox init
	cabal update
	cabal install alex
	cabal install happy
.if "YES" == "${USE_GTK}"
	cabal install gtk2hs-buildtools
.endif

# clean up object files
.PHONY: clean
clean:
	cabal clean


# clean everything
.PHONY: complete-clean
complete-clean: clean
	${RM} -r ${CABAL_SANDBOX}
	${RM} cabal.sandbox.config

# emacs interface
.PHONY: emacs-1 emacs-2 emacs-9
emacs-1: run
emacs-2: deps
emacs-9: setup
