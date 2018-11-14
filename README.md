# Git time machine

![Timemachine](timemachine.gif)

## Installation

[![MELPA](http://melpa.org/packages/git-timemachine-badge.svg)](http://melpa.org/#/git-timemachine)

Installation alternatives:

- Download git-timemachine.el and drop it somewhere in your `load-path`.
- If you use `el-get`, simply add `git-timemachine` to your packages list.
- If you have melpa configured it's available through `package-install`.

## Usage

Visit a git-controlled file and issue `M-x git-timemachine` (or bind
it to a keybinding of your choice). If you just need to toggle the
time machine you can use `M-x git-timemachine-toggle`.

Use the following keys to navigate historic version of the file
 - `p` Visit previous historic version
 - `n` Visit next historic version
 - `w` Copy the abbreviated hash of the current historic version
 - `W` Copy the full hash of the current historic version
 - `g` Goto nth revision
 - `t` Goto revision by selected commit message
 - `q` Exit the time machine.
 - `b` Run `magit-blame` on the currently visited revision (if magit available).
 - `c` Show current commit using magit (if magit available).

## Customize

Set `git-timemachine-abbreviation-length` (default 12) to your
preferred length for abbreviated commit hashes.

Set `git-timemachine-show-minibuffer-details` (default t) to control
whether details of the commit are shown in the minibuffer.

Also `M-x customize [git-timemachine]`.

## FAQ

### How do I "rollback" to the currently visited revision?

Just use your normal `write-file` and supply the file name of your choice. Be aware that you might be throwing away local uncommitted changes to the file in question. If you don't want to use you muscle memory to write the file you can map `write-file` to something short and sweet in `git-timemachine-mode-map`.
