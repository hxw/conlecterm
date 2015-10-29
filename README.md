# conlecterm - a simple tabbed terminal

A simple frame for embedding applications that support the XEmbed
protocol.  The program uses the GTK Socket class to generate a window
for the embedded application to use.

The display uses a GTK Notebook to collect multiple applications into
a single top level window.

Each group of applications is termed a _session_ and multiple sessions
can be defined each having the option of a different tab orientation.

Tabs can be moved by draggng them to the desired position.

The order of the tabs is saved in the session file when close button is clicked.

# Configuration

the configuration file has three kinds of configuration elements

- Commands

  A list of strings, the first being the program.  Any string may
  optionally contain the Window ID or tab text.


- Panes

  Define the text that appears on the tab, the command to run, the
  initial directory.


# Programs using XEmbed

## Working

- urxvt works
- emacs works

## Partially working

- gvim  (Ubuntu: vim-gtk) very slow (30 sec) to start shows menu proxy timeout. Seems to be an Ubuntu bug.


## Not working

- xterm not accepting focus, but X cut and paste works.


# Compiling

The program is written in Haskell and requires GTK and X11

Haskell packages:

Operating System  Packages
----------------  --------
Ubuntu            libghc-gtk-dev libghc-x11-dev libghc-parsec3-dev libghc-hashtables-dev libghc-aeson-dev
FreeBSD           hs-gtk2hs hs-X11 hs-parsec hs-hashtables hs-aeson


Installing the above packages should bring in the right dependencies.

A simple Makefile is provided.

# Building with cabal

~~~
cabal sandbox init
cabal update
cabal install alex
cabal install happy
cabal install gtk2hs-buildtools
cabal install --dependencies-only
cabal build
~~~

# Configuration

Configuration directory is searched from:

- `${XDG_HOME}/conlecterm`
- `${HOME}/.config/conlecterm`
- `${HOME}/.conlecterm`

Then first directory found is used and this is checked for a
configuration file.

The configuration is loaded from a file in the above directory called
`config.rc`.

The program will not start if a valid directory is not found or the
directory does not contain a `config.rc`.

Saved session files are called `SESSION.session` where *SESSION* is
replaced by the session name.  The format of this file JSON.

# Bugs


# TODO

- add the _send_ function
