# Makefile

# basic settings
PROGRAM = Conlecterm

# need to include gtk2hs
USE_GTK = YES

# a default agument list for run - averride as required
PROG_ARGS ?= -c .

.include "Mk/haskell.mk"
