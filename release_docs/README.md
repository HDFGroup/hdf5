# The `release_docs` directory

This directory contains **release artifacts only**: changelogs, version history,
release process documentation, and maintainer information.

User-facing guides (installation, build instructions, platform-specific docs)
have been moved to the [`docs/`](../docs/) directory.

## Contents

### CHANGELOG.md (formerly RELEASE.txt)

This is the changelog for the current version of the library.

For a MAJOR release (or in `develop`) this file lists all the changes since the
last major version. For a MINOR release (or in a maintenance branch), this file
lists all the changes since the last release in the maintenance branch.

Examples:

* The file for HDF5 1.14.0 includes all the changes since HDF5 1.12.0
* The file for HDF5 1.10.9 includes all the changes since HDF5 1.10.8
* The file in `develop` includes all the changes since the last major release
* The file in `hdf5_1_14` includes all the changes since the last minor HDF5 1.14 release

### HISTORY files

The `HISTORY` files contain the history of this branch of HDF5. They fall into
three categories.

#### HISTORY-\[VERSION 1\]-\[VERSION 2\].txt

These files are created when we release a new major version and include all
the changes that were made to the `develop` branch while creating a major release.

#### HISTORY-\[VERSION\].txt

This file contains the changes that were made to a maintenance branch since
it split off from `develop`. It will also be found in the `develop` branch
when experimental releases have been created.

Note that we make no effort to bring maintenance branch `HISTORY` files back to
develop. If you want to compare, say, 1.10.4 with 1.12.3, you'd have to get
the history files from those releases and compare them by hand.

### RELEASE_PROCESS.md

Documentation for how releases are created and managed.

### MAINTAINERS.md

Maintainer information for the project.

## Creating new releases

### MAJOR release

* If there were experimental releases, merge the experimental `HISTORY` file
  and the current `CHANGELOG.md` by category to create a separate, unified
  file that ignores the experimental releases. Don't check this in yet or
  clobber any existing `HISTORY`/`RELEASE` files, but put it someplace handy for
  use in later steps.

* Create the new maintenance branch

In develop:
* Create the new `HISTORY-\[VERSION 1\]-\[VERSION 2\].txt` file
    * If there is an experimental `HISTORY` file, add `CHANGELOG.md` to the beginning of it and use that
    * Otherwise, start with `CHANGELOG.md`
    * Add the introduction boilerplate like in the other `HISTORY` files (TOC, etc.)
* Delete any experimental `HISTORY` file
* Clear out `CHANGELOG.md`

Note that we're KEEPING any experimental release history information in the
`HISTORY-\[VERSION 1\]-\[VERSION 2\].txt` file, so do NOT use the merged file in
the above steps!

In the new maintenance branch:
* Create the new `HISTORY-\[VERSION\].txt` file
    * If there is an experimental `HISTORY` file use the combined file you created earlier
    * Otherwise, start with `CHANGELOG.md`
    * Add the introduction boilerplate like in the other `HISTORY` files (TOC, etc.)
* Delete any experimental `HISTORY` file
* Clear out `CHANGELOG.md`

* Create the new release branch

In the new release branch:
* If there were experimental releases, use the combined file you created earlier as `CHANGELOG.md`
* Otherwise the `CHANGELOG.md` will be used as-is

### MINOR release

* Create the release branch

In the maintenance branch:
* Add the contents of `CHANGELOG.md` to the beginnnig of `HISTORY-\[VERSION\].txt`
* Clear out `CHANGELOG.md`

### EXPERIMENTAL release

* Add the contents of `CHANGELOG.md` to the beginnnig of `HISTORY-\[VERSION\].txt`
* Clear out `CHANGELOG.md`
