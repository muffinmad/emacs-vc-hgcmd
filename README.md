[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/copyleft/gpl.html)

# emacs-vc-hgcmd

Emacs VC backend to work with mercurial repositories through [hg command server](https://www.mercurial-scm.org/wiki/CommandServer)

## Advantage over `vc-hg`

The main advantage compared to `vc-hg` is speed.

Because communicating with single `hg` process over pipe is much faster than starting separate `hg` process for each command.

### Other improvements and differences

#### File renames and short log

`vc-hgcmd` can't show file renames in `vc-dir` and doesn't have short log version yet.

#### Unresolved conflict status for a file

Files with unresolved merge conflicts have appropriate status in `vc-dir`.
Also you can use `vc-find-conflicted-file` to find next file with unresolved merge conflict.

#### hg summary as `vc-dir` extra headers

`hg cummary` command gives useful information about commit, update and phase states.

#### Current branch is displayed on mode line.

Mode-line format is not customizable yet.

#### Amend and close branch commits

While editing commit message you can togle `--amend` and `--close-branch` flags.

#### Merge branch

`vc-hgcmd` will ask for branch name to merge.

#### Default pull argements

You can customize default `hg pulll` command arguments.
By default it's `--update`. You can change it for particular pull by invoking `vc-pull` with prefix argument.

#### Branches and tags as revision completion table

Instead of list of all revisions of file `vc-hgcmd` provides list of named branches and tags.
It's very useful on `vc-retrieve-tag`.
You can specify `-C` to run `hg update` with `-C` flag and discard all uncommited changes.

#### Filenames in `vc-annotate` buffer are hidden

They are needed to annotate changes across file renames but mostly useless in annotate buffer.
`vc-hgcmd` removes it from annotate buffer but keep it in text properties.

#### Create tag

`vc-hgcmd` creates tag on `vc-create-tag`.
If `vc-create-tag` is invoked with prefix argument then named branch will be created.

#### Predefined commit message

While commiting merge changes commit message will be set to `merged <branch>` if
different branch was merged or to `merged <node>`.

## Installation

Download `vc-hgcmd.el` and run:

<kbd>M-x</kbd> `package-install-file` <kbd>RET</kbd> `<path-to-vc-hgcmd-el>` <kbd>RET</kbd>

Also you need to add `Hgcmd` to `vc-handled-backeds` before or instead `Hg`.
