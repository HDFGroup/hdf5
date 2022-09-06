# Getting Started with HDF5 Development

## A brief tour of the source code

Here's a quick guide to where you can find things in our source tree. Some of these directories have README.md files of their own.

`bin/`
Scripts we use for building the software and misc. tools.

`c++/`
Source, tests, and examples for the C++ language wrapper.

`config/`
Configuration files for both the Autotools and CMake.

`doc/`
Miscellaneous documents, mostly in markdown format.

`doxygen/`
Mainly Doxygen build files and other top-level Doxygen things. The Doxygen content is spread across the library's header files but some content can be found here when it has no other obvious home.

`examples/`
C library examples. Fortran and C++ examples are located in their corresponding wrapper directory.

`fortran/`
Source, tests, and examples for the Fortran language wrapper.

`hl/`
Source, tests, and examples for the high-level library.

`java/`
Source, tests, and examples for the JNI language wrapper and the corresponding OO Java library.

`m4/`
m4 build scripts used by the Autotools. CMake ignores these.

`release_docs/`
Install instructions and release notes.

`src/`
Source code for the C library.

`test/`
C library test code. Described in much more detail below.

`testpar/`
Parallel C library test code. Described in much more detail below.

`tools/`
HDF5 command-line tools code, associated tools tests, and the input test files.

`utils/`
Small utility programs that don't belong anywhere else.


## General Things

### Platform-independence

HDF5 assumes you have a C99 compiler and, to a certain extent, a POSIX-like
environment (other languages will be discussed later). On most operating systems
in common use, this will be a reasonable assumption. The biggest exception to
this has been Windows, which, until recently, had poor C99 compliance and spotty
POSIX functionality. To work around differences in platforms and compilers,
we've implemented a compatibility scheme.

Unlike most codebases, which test for features and inject their own normal-looking
functions when there are deficiencies, HDF5 uses a scheme where we prefix all
C and POSIX calls with `HD` (e.g., `HDmalloc`). The `H5private.h` header handles
most of the fixup for Unix-like operating systems and defines the HD replacements.
For Windows, we first parse the `H5win32defs.h` file, which maps missing Windows
and MSVC functionality to POSIX and C99 calls. `H5private.h` tests for previously
defined HD calls and skips redefining it if it already exists. H5system.c
includes Windows glue code as well as a few functions we need to paper over
differences in Unix-like systems.

One thing to keep in mind when looking at our platform-independence layer is
that it is quite old, having been authored when the Unix world was much more
fragmented and C99 was uncommon. We've slowly been reworking it as POSIX has
standardized and C99 become widespread.

Another thing to keep in mind is that we're fairly conservative about deprecating
support for older platforms and compilers. There's an awful lot of ancient
hardware that requires HDF5, so we try to only make major changes to things
like language requirements when we increment the major version (minor version
prior to HDF5 2.0).

### C99

We assume you have a C99 compiler. Subfiling uses some C11 features, but that is
compiled on demand and we are not moving that requirement to the rest of the
library. All modern compilers we've tested can handle HDF5's C99 requirements,
even Microsoft's. In the future, we'll remove the `HD` prefixes from all standard
C library calls.

One quirk of HDF5's age, is the `hbool_t` type, which was created before C99
Booleans were widespread and which uses `TRUE` and `FALSE` macros for its values
instead of C99's `true` and `false`. We plan to switch this over to C99's Boolean
types sometime in the near future.

### POSIX

We assume basic POSIX.1-2008 functionality is present. When a POSIX (or common Unix)
function is missing on a popular platform, we implement a shim or macro
in `H5private.h` and/or `H5system.c`. Systems that need a lot of help, like
Windows, have gotten special headers in the past (e.g., `H5win32defs.h`) but
now that most platforms implement the POSIX and C99 functionality we need, these
special headers are less necessary.

### Threads

Thread-safety was originally implemented using Pthreads, with Win32 support
bolted on later. No other thread libraries are supported. The subfiling
feature uses multiple threads under the hood, but that's out of scope for
an introductory document. Thread-related code is largely confined to the `H5TS`
files, where we define HDF5-specific primitives and then map Pthread or Win32
implementations onto them.

### C++

The C++ Wrappers require C++11. We generally only require the rule of three
for the classes.

## Fortran

The Fortran wrappers require Fortran 2003.

## Java

The Java wrappers require Java 8.

## Warning suppression

In the rare cases where we've decided to suppress a warning, we have a set
of function-like macros that we use for that. They are located in `H5private.h`
and have the form `H5_<compiler>_DIAG_(OFF|ON)` and take the name of the warning
they are suppressing as a parameter. They were originally designed for gcc and
extended to clang. They have not been updated for other compilers. Instead,
we have plans to revamp the macro system to be more generic and extensible.

We try to configure the compilers we use the most for maximum grumpiness and
then fix all the warnings we can. Please don't use the warning suppression
macros in lieu of actually fixing the underlying problems.


