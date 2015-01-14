el-get-lock.el --- Lock [El-Get][] package repository versions
==============================================================

## Installation

If you don't have [El-Get][] installed, first install it.  After that,
place the following command in your init file and restart your Emacs.

```lisp
(el-get-bundle tarao/el-get-lock)
```

## Usage

To enable version locks for all packages, run `el-get-lock` command
without an argument.  This can be done by `M-x el-get-lock` or placing
the following code in your init file.

```lisp
(el-get-lock)
```

If you want to lock only a few packages, then pass them to
`el-get-lock` command.  You can use the command multiple times. (All
the packages passed to the command are marked as locked.)

```lisp
(el-get-lock 'evil 'anything)
(el-get-lock 'magit)
```

You can also specify packages not to lock by `el-get-lock-unlock` command.

```lisp
(el-get-lock)
(el-get-lock-unlock 'undo-tree)
```

Note that `el-get-lock` or `el-get-lock-unlock` without an argument
resets the locked/unlocked package list and marks all the packages as
locked/unlocked respectively.

If a package is marked as locked, a new installation of the package by
[El-Get][] will be locked to a version which is registered in a lock
file (`~/.emacs.d/el-get.lock`).  If there is no version registered,
the latest version is installed and the version is registered to the
lock file.

## Recommended Workflows

### Generate lock file

Assume that you have already run `el-get-lock` by hand or in your init
file.  After that, `el-get-install` or `el-get-update` will
automatically maintain the lock file but if you have packages already
installed before running `el-get-lock`, they need to be reinstalled or
updated to get registered to the lock file.  `M-x el-get-reinstall`,
`M-x el-get-update`, or `M-x el-get-update-all` does this for you.

The lock file is `~/.emacs.d/el-get.lock` by default and customizable
by `el-get-lock-file` variable.  It is good to have the lock file
version controlled by for example the following list of commands,
which is for [`git`][git].

```
cd ~/.emacs.d/
git add el-get.lock
git commit -m 'Add el-get lock file.'
```

### Install locked versions on another machine

When you restore the installation of packages by `el-get` function
written in your init file, they are locked as long as your init file
properly initializes `el-get-lock` as described above.

If you have packages already installed by `el-get` before using
`el-get-lock` and the initialization of `el-get-lock` and its lock
file comes from another machine (via `git pull`-ing your configuration
for example), you need to synchronize your installed packages to
locked versions by `M-x el-get-lock-checkout`.  In this case, neither
`el-get-update` nor `el-get-update-all` is suitable since they may
update your package to a newer version than the locked version.

### Update a package

`M-x el-get-update` or `M-x el-get-update-all` will update your
packages as usual.  After that, you will see that your version
controlled lock file have been changed.  If the updated versions work
fine, then commit your lock file.

```
cd ~/.emacs.d/
git add el-get.lcok
git commit -m 'Update some packages.'
```

### Cancel updating a package

If an updated package breaks your environment and you want it to
revert, then revert the lock file first. This will cancel registering
the updated versions.

```
cd ~/.emacs.d/
# This will revert the entire update; you may need to manually modify
# the lock file if you have multiple packages updated and not all of
# them are to revert
git checkout el-get.lock
```

Then, revert the packages by `M-x el-get-lock-checkout` and now you
have versions before the update.

## Supported Types

### Full support

These types are fully supported.  Packages of these types can be
locked, updated and reverted.

- `git`
- `github`
- `emacsmirror`
- `hg`

### Partial support

These types are partially supported.  Packages of these types can be
locked and updated but cannot be reverted.  If you try to revert the
package by `el-get-lock-checkout`, the package is simply reinstalled,
i.e., the latest version from a remote host get installed.

- `http`
- `ftp`
- `emacswiki`

Note that the first locked installation for a package of these types
yields an error when the remote version gets newer than the locked
version.  The only way to get rid of the error is to accept the latest
version by `el-get-lock-checkout`.

## Reference

### Customization

- `el-get-lock-file` : file

  File to store the information of previously installed versions.

### Commands

- `el-get-lock` (*packages*)

  Lock El-Get repository versions of *packages*.

  If *packages* are specified, those *packages* are marked to be
  locked.  Otherwise, the all installed packages and packages to be
  installed in future are locked.

  Packages marked as locked and their dependent packages are locked
  to the version stored in `el-get-lock-file`.  When `el-get`
  installs a package for the first time, the repository version is
  saved to `el-get-lock-file`.  Next time you call `el-get` for the
  package, the repository version of the package is locked
  according to the value in the `el-get-lock-file`.

  Calling `el-get-update` for a package will change the stored
  value of `el-get-lock-file` to the latest version.

- `el-get-lock-unlock` (*packages*)

  Unlock El-Get repository versions of *packages*.

  If *packages* are specified, those *packages* are marked to be
  unlocked.  Otherwise, the all installed packages are unlocked.

- `el-get-lock-checkout` (*packages*)

  Checkout the locked version of *packages* or simply the latest
  version is reinstalled for some unsupported package types.

[El-Get]: http://github.com/dimitri/el-get
[git]: http://git-scm.com/
