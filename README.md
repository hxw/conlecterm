# conlecterm - a simple tabbed terminal

A simple frame for embedding applications that support the XEmbed
protocol.  The program uses the GTK Socket class to generate a window
for the embedded application to use.

The display uses a GTK Notebook to collect multiple applications into
a single top level window.

Each group of applications is termed a _session_ and multiple sessions
can be defined each having the option of a different tab orientation.


# Configuration

the configuration file has three kinds of configuration elements

- Commands

  A list of strings, the first being the program.  Any string may
  optionally contain the Window ID or tab text.


- Panes

  Define the text that appears on the tab, the command to run, the
  initial directory.  (*TODO* send command to program, manual start)


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


# Bugs

- Unity launcher icon not working - cannot switch focus. Could this be associated with xterm focus problem?
- Unity full screen does not get close button on global menu


# TODO

- add the _send_ function
- make the "restart/close" dialog embed in the pane
- add the manual start option (requires previous item)
- how and which screen position to implement _buttons_ (a special tab called NEW?)
