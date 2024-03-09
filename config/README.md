# The `config` directory

## Intro

HDF5 can be configured using both the GNU Autotools and CMake. We try to keep
them in sync, but you can expect minor differences to crop up. Please create
a GitHub issue for any differences noted. Note that with the Autotools, we
do NOT check generated files into GitHub until release time, so you will
need to generate `configure`, `Makefile.in`(s), etc. via `autogen.sh` in the
project root if you want to build with that system.

Configuration information for the HDF5 library and tools is (unfortunately)
spread across the repository. Basic library configuration will generally
be found in `configure.ac` (Autotools) and the root's `CMakeLists.txt` (CMake).
Each subdirectory of the project also has its own `Makefile.am` or CMake build
and test files.

This directory contains a few important things:

* Autotools OS- and compiler-specific configuration
* CMake support files (in `cmake`)
* Warning files shared between the two systems (in `*-warnings` directories)
* CMake toolchain files (in `toolchain`)
* CMake sanitizer files (in `sanitizer`)

CMake will be documented elsewhere. This document focuses on the Autotools files
and the shared warning files.

## Autotools

An Autotools build will first use `$host_cpu`, `$host_os`, etc. to try to find a
suitable platform file in `config` to source and start checking compilers. The
code that does this is in `configure.ac` (search for `host_os`). For example,
MacOS will source the `apple` file and FreeBSD will source the `freebsd` file.

If you dig into one of these files, the way that they check for compilers is
rather crude. Each OS script will simply source the various C, C++, and
Fortran compiler files that are listed inside. Each compiler file checks
the designated compiler's version output to see if there's a match, and if so,
the flag processing proceeds, and a variable like `cc_flags_set` will be set
at the end.

In case it's not obvious, the C files end in `-flags`, C++ in `-cxxflags`, and
Fortran in `-fflags`.

When a compiler matches, the script will attempt to set the `CFLAGS`, etc.
variables based on the platform and compiler's properties. There are typically
a large number of flag categories (e.g., `DEBUG_OPT_CFLAGS`) that are
conditionally appended to the canonical variables, like `AM_FLAGS`, by the
remainder of the `configure` script.

For the major compilers, like Clang and gcc, there will be a section at the
end where we append version-specific flags, mainly for warnings. These are
imported via a function in the script (`load_gnu_arguments()` for gcc). See
below for more detail.

## Warnings files

Keeping the Autotools and CMake build files in sync has always been a bit of a
struggle. One way that we help to ensure that the same flags are used in each
build system is to import the warnings settings from text files that are
maintained separately from the Autotools and CMake build files. We like to
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
