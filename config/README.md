# The `config` directory

## Intro

HDF5 can be configured using CMake.

Configuration information for the HDF5 library and tools is (unfortunately)
spread across the repository. Basic library configuration will generally
be found in the root's `CMakeLists.txt` for CMake.
Each subdirectory of the project also has its own CMake build
and test files.

This directory contains a few important things:

* CMake support files (in `cmake`)
* Warning files in `*-warnings` directories
* CMake toolchain files (in `toolchain`)
* CMake sanitizer files (in `sanitizer`)

## Warnings files

We like to
configure the compiler to be as crabby as possible so as to catch subtle bugs,
so there are a LOT of warning flags for popular compilers like Clang and gcc.

We've located these files in `config/*-warnings` directories. Each file
represents a compiler version and contains the warning flags we set, one to a
line. Lines that start with `#` are considered comment lines. You'll also see
`developer` and `no-developer` flavors of compiler version files. The former
corresponds to "developer flags" that are usually either only semi-useful and/or
generate a lot of (usually unfixable) noise. The latter corresponds to things
that we want to ensure do NOT appear in non-developer builds of the library.
These might involve a different level setting (`-Wfoo=x`) or something that
gets incorporated in a "conglomerate" flag like `-Wextra` so we need to set
`-Wno-foo` in non-developer builds. Developer warnings can be turned on
via a configure option. You will also sometimes see `error` files. Those are
files that include warnings that will be considered errors if you have enabled
the "warnings as errors" configure option set. Now that the library is largely
warning-free, these are less useful than in the past as you can now just set
-Werror directly in many cases (our configure script is smart about not running
configure checks with -Werror).

For anyone interested, we are always interested in improving both the OS and
compiler files, so pull requests for those are always welcome, especially for
platforms we don't have routine access to. If you are a compiler or platform
expert/aficionado, please help us out!
