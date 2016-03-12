# nix-proj

A silly little program to keep track of project related files that you
don't want to have in the source repo in one central place. Right now
just the standard `{default,shell,release}.nix` files.

## Usage

To fix or initialize a project, i.e. symlink in the relevant files do:

```
nix-proj mk
```

To list the current number of tracked projects do:

```
nix-proj ls
```

To save changes made to the nix-proj related project files do:

```
nix-proj ci
```

## Installation

First install nix then issue:

```
nix-env -if release.nix
```

## QA

* This is really simple to do with a shell script
    * I know..
* But you have nix files in this project, shouldn't we keep them with the source?
    * All projects are different, the consumers of this will likely have nix installed and know how to use it
