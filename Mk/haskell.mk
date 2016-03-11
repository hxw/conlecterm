# BSDmakefile

# basic settings
PROGRAM ?= SetMe

# need to include gtk2hs
USE_GTK ?= NO

# a default agument list for run - override as required
PROG_ARGS ?= -v -c ${PROGRAM}.conf

# default location of cabal built binary
PROG_BIN ?= ./dist/build/${PROGRAM}/${PROGRAM}

# desktop file
HAS_DESKTOP_FILE ?= NO
PROG_DESKTOP ?= ${PROGRAM}.desktop

# installation directory
INSTALL_DIR ?= ${HOME}/bin
APPLICATIONS_DIR ?= ${HOME}/.local/share/applications

# the default cabal file and related items
CABAL_FILE ?= ${PROGRAM}.cabal
CABAL_SANDBOX = .cabal-sandbox
CABAL_CONFIG ?= cabal.config
CABAL ?= cabal --config-file='${CABAL_CONFIG}'

# utilities
RM = rm -f
SAY = echo
SED = sed
INSTALL_PROGRAM ?= install -C -s -m 0755
INSTALL_DATA  ?= install -C -m 0644

# default target
.PHONY: all
all: setup deps
	${CABAL} build

.PHONY: help
help:
	@${SAY} 'list of make targets for application: "${PROGRAM}"'
	@${SAY} '  all             - build the application'
	@${SAY} '  deps            - update sandbox if "${CABAL_FILE}" was changed'
	@${SAY} '  run             - run the application (build if necessary)'
	@${SAY} '  test            - run the unit tests'
	@${SAY} '  install         - install the application binary in "${INSTALL_DIR}" (build if necessary)'
	@${SAY} '  setup           - install the cabal-sandbox if not already installed'
	@${SAY} '  update          - update the cabal index files'
	@${SAY} '  doc             - build documentataion'
	@${SAY} '  list-repo       - list all items downloaded to local-cache'
	@${SAY} '  clean           - clean up application object/binary files'
	@${SAY} '  clean-sandbox   - clean then destroy sandbox'
	@${SAY} '  clean-repo      - remove local repo cache'
	@${SAY} '  complete-clean  - run all the above cleans'


# run the program with supplied arguments
.PHONY: run
run: all
	${PROG_BIN} ${PROG_ARGS}

# install any new dependencies
# run this if  the .cabal file changes
.PHONY: deps
deps: ${CABAL_FILE}
	${CABAL} install --only-dependencies --enable-documentation --enable-tests

# install the application binary
.PHONY: install pre-install do-install post-install
install: pre-install do-install post-install
pre-install:
do-install:
	@if [ ! -d '${INSTALL_DIR}' ]; then ${SAY} 'missing directory: ${INSTALL_DIR}'; false; fi
	${INSTALL_PROGRAM} '${PROG_BIN}' '${INSTALL_DIR}/${PROGRAM}'
.if "YES" == "${HAS_DESKTOP_FILE}"
	@if [ ! -d '${APPLICATIONS_DIR}' ]; then ${SAY} 'missing  directory: ${APPLICATIONS_DIR}'; false; fi
	${INSTALL_DATA} '${PROG_DESKTOP}' '${APPLICATIONS_DIR}/${PROG_DESKTOP}'
.endif
post-install:

# documentation
.PHONY: doc
doc: all
	${CABAL} haddock --all --internal --hyperlink-source
	${CABAL} hscolour --all

# run tests
.PHONY: test
test:
	${CABAL} test

# install sandbox if not already installed and build
.PHONY: sandbox-init
sandbox-init:
	@[ -d .cabal-sandbox ] || ${MAKE} setup

# just update cabal index
.PHONY: update
update:
	${CABAL} update

# setup sandbox and install required tools
.PHONY: setup
setup: ${CABAL_SANDBOX}

${CABAL_SANDBOX}:
	${CABAL} sandbox init
	${CABAL} update
	${CABAL} install alex
	${CABAL} install happy
.if "YES" == "${USE_GTK}"
	${CABAL} install gtk2hs-buildtools
.endif

# list the local repository pacakages with versions
# this is all packackes currently cached
# (use "-not -depth 1" to ignore the index files)
.PHONY: list-repo
list-repo:
	@find local-cache/hackage.haskell.org -type f -not -depth 1 -name '*.tar.gz' \
	 | sort \
	 | sed -E 's@^.*/([^/]+)-([.[:digit:]]+)\.tar\.gz$$@\1==\2@'

# clean up object files
.PHONY: clean
clean:
	${CABAL} clean


# remove sandbox
.PHONY: clean-sandbox
clean-sandbox: clean
	${RM} -r ${CABAL_SANDBOX}
	${RM} cabal.sandbox.config

# remove repo cache
.PHONY: clean-repo
clean-repo:
	${RM} -r local-cache

# clean everything
.PHONY: complete-clean
complete-clean: clean clean-sandbox clean-repo

# emacs interface
.PHONY: emacs-1 emacs-2 emacs-3 emacs-5 emacs-9
emacs-1: run
emacs-2: deps
emacs-3: update
emacs-5: doc
emacs-9: setup

.PHONY: test-1
test-1: test
