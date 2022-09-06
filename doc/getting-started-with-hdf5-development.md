# Getting Started with HDF5 Development

## Introduction


## Getting started

### Building the library for development

You don't really need special configuration settings for building the library
as a developer.

Some tips that may be useful:

* Building in debug mode will turn on additional checks in many packages.
  You'll probably want to start coding in debug mode.
* You can turn symbols on independently of debug/production mode.
* If you will be looking at memory issues via tools like valgrind, you will
  need to turn off the free lists, which recycle memory so we can avoid
  calling malloc/free. This is done using the `--enable-using-memchecker`
  configure option. Some developers build with this all the time, as the
  memory recyclilng can hide problems like use-after-free.
* You can enable developer warnings via `--enable-developer-warnings`. These
  warnings generate a lot of noise, but the output can occasinally be useful.
  I probably wouldn't turn them on all the time, though, as they can make it
  harder to spot warnings that we care about.
* You can set warnings as errors. We have an older scheme that does this for
  a subset of errors or you can simply specify `-Werror`, etc. as a part of
  `CFLAGS`. Configure is smart enough to strip it out when it runs configure
  checks. We build the C library with -Werror on GitHub, so you'll need to fix
  your warnings before creating a pull request.
* CMake has a developer mode that turns most these settings on.


### Branches

We have a document in the `doc` directory that explains our branching strategy.
For new small features, we have developers create feature branches in their own
repositories and then create pull requests into the canonical HDF5 repository.
For larger work, especially when the work will be done by multiple people, we
create feature branches named `feature/<feature>`. If work stops on a feature
branch, we rename it to `inactive/<feature>`.

If you create a feature branch, please create a `BRANCH.md` text file in the
repository root and explain:

* The branch's purpose
* Contact info for someone who can tell us about the branch
* Clues about when the branch will be merged or can be considered for retirement


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

### Fortran

The Fortran wrappers require Fortran 2003.

### Java

The Java wrappers require Java 8.

### Warning suppression

In the rare cases where we've decided to suppress a warning, we have a set
of function-like macros that we use for that. They are located in `H5private.h`
and have the form `H5_<compiler>_DIAG_(OFF|ON)` and take the name of the warning
they are suppressing as a parameter. They were originally designed for gcc and
extended to clang. They have not been updated for other compilers. Instead,
we have plans to revamp the macro system to be more generic and extensible.

We try to configure the compilers we use the most for maximum grumpiness and
then fix all the warnings we can. Please don't use the warning suppression
macros in lieu of actually fixing the underlying problems.

## Build Systems

We support building the library with both the Autotools and CMake. We'd like to
eventually move to only having one build system, which would be CMake since
the Autotools don't really support Windows, but that seems unlikely to happen
anytime soon. With few exceptions, any new feature, test, or configure
option should be supported in both build systems.

The particulars of the build systems can be found in the `config` directory
and its subdirectories.

## Working in the library

### Anatomy of an HDF5 API call

HDF5 API calls have a uniform structure imposed by our function enter/leave and
error handling schemes. We currently stick to this boilerplate for ALL
functions, though this may change in the future. The general boilerplate varies
slightly between private and public API calls.

Here's an example of an internal API call:

```c
herr_t
/*
 * Function comments of dubious value
 */
H5X_do_stuff(<parameters>)
{
	<variables>
	void *foo = NULL;
	herr_t ret_value = SUCCEED;

	FUNC_ENTER_NOAPI(FAIL)

	HDassert(<parameter check>);

	if (H5X_other_call() < 0)
		HGOTO_ERROR(H5E_MAJ, H5E_MIN, FAIL, "badness")

done:
	if (ret_value < 0)
		<do error cleanup>
	<do cleanup stuff>

	FUNC_LEAVE_NOAPI(ret_value);
}
```

There are a couple of things to note here.

* Each function call has a header comment block. The information you'll find in
  most function comments is not particularly helpful. We're going to improve the
  format of this. 
* Most functions will return `herr_t` or a `hid_t` ID. We try to avoid other
  return types and instead use out parameters to return things to the user.
* The name will be of the form `H5X_do_something()` with one or two underscores
  after the `H5X`. The naming scheme will be explained later.
* Even though C99 allows declarations anywhere in the function, we put most of
  them at the top of the file, with the exception of loop variables and
  variables that are "in scope" inside an ifdef.
* We generally initialize values that may need to be freed or otherwise cleaned
  up to a "bad" value like `NULL` or `H5I_INVALID_HID` so we can better clean up
  resources on function exit, especially when there have been errors.
* Most non-void functions will declare a variable called `ret_value`. This is
  used by the error handling macros. It's usually the last thing declared in
  the variable block.
* Every function starts with a `FUNC_ENTER macro`, discussed later in this
  document.
* Most internal calls will check parameters via asserts.
* We check the return value of any call that can return an error, using the form
  shown.
* On errors, an error macro is invoked. These are described later in this
  document.
* Any function that returns an error will have a `done` target. Most error
  macros jump to this location on errors.
* We do most cleanup at the end of the function, after the `done` target. There
  are special `DONE` flavors of error macro that we use in post-`done` cleanup
  code to detect and report errors without loops.
* Almost every function ends with a `FUNC_LEAVE` macro.

And here's an example of a public API call:

```c
/*
 * Doxygen stuff goes here
 */
herr_t
H5Xdo_api_stuff(<parameters>)
{
	<variables>
	herr_t ret_value = SUCCEED;

	FUNC_ENTER_API(FAIL)
	H5TRACE3(<stuff>)

	if (<parameter check)
		HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "badness")

	<VOL setup>

	if (H5VL_call() < 0)
		HGOTO_ERROR(H5E_FOO, H5E_BAR, FAIL, "badness")

done:
	if (ret_value < 0)
		<do error cleanup>
	<do cleanup stuff>

	FUNC_LEAVE_API(ret_value);
}

```

A public API call differs little from an internal call. The biggest differences:

* Public API calls are commented using Doxygen. This is how we generate the
  reference manual entries.
* The name will have the form `H5Xdo_stuff()`, with no underscores after the `H5X`.
* The function enter macro is `FUNC_ENTER_API` (or similar). Under the hood, this
  one differs quite a bit from an internal function enter macro. It checks for
  package initialization, for example, and acquires the global lock in thread-safe HDF5.
* There is a `TRACE` macro. This helps with API tracing and is applied by a
  script invoked by `autogen.sh` (Autotools) or CMake. You probably don't need
  to worry much about this.
* Parameter checking uses the regular HDF5 error scheme and invokes 
  `HGOTO_ERROR` macros on errors.
* In storage-related calls, there will usually be some VOL setup (HDF5 1.12.x
  and later) and in lieu of a regular internal API call, there will be an `H5VL`
  VOL call.
* The function exit macro will be `FUNC_LEAVE_API` (or similar). This is where
  we release the global thread-safe lock, etc.

