# Git time machine

![Timemachine](timemachine.gif)

## Installation

Installation alternatives:

- Download git-timemachine.el and drop it somewhere in your `load-path`.
- If you use `el-get`, simply add `git-timemachine` to your packages list.
- If you have melpa configured it's available through `package-install`.

## Usage

Visit a git-controlled file and issue `M-x git-timemachine` (or
bind it to a keybinding of your choice).

Use the following keys to navigate historic version of the file
 - `p` Visit previous historic version
 - `n` Visit next historic version
 - `w` Copy the hash of the current historic version
 - `q` Exit the time machine.
