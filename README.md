# conlecterm - a simple tabbed terminal

A simple frame for embedding applications that support the XEmbed
protocol.  The program uses the GTK Socket class to generate a window
for the embedded application to use.

The display uses a GTK Notebook to collect multiple applications into
a single top level window.

Each group of applications is termed a _session_ and multiple sessions
can be defined each having the option of a different tab orientation.

Tabs can be moved by draggng them to the desired position.

The order of the tabs is saved when close button is clicked. 

# Configuration

the configuration file has three kinds of configuration elements

- Commands

  A list of strings, the first being the program.  Any string may
  optionally contain the Window ID or tab text.


- Panes

  Define the text that appears on the tab, the command to run, the
  initial directory.


- Sessions

  Name a group of panes that are opened together, a command-line
  argument specifies the name.


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

Ubuntu packages:
 
- libghc-gtk-dev
- libghc-x11-dev
- libghc-parsec3-dev

A simple Makefile is provided.

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
directory does not conatin a `config.rc`. 

Saved session files are called `session-SESSION.rc` where *SESSION* is
replaced by the session name.  The format of this file is the same as
a session block from the main configuratiopn file.  Session fils
simple override the session in the main configuration, so to return to
the initial session simple delete the asppropriate session file. 

# Bugs


# TODO

- add the _send_ function
