# Getting Started with HDF5 Development

## Introduction

The purpose of this document is to introduce new HDF5 developers to some of the
quirks of our source code. It's not a style guide (see the forthcoming HDF5
style guide for that), but instead describes the most commonly encountered
features that are likely to trip up someone who has never worked with the HDF5
source code before.

Corrections and suggestions for improvement should be handled via GitHub pull
requests and issues.

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
  warnings generate a lot of noise, but the output can occasionally be useful.
  I probably wouldn't turn them on all the time, though, as they can make it
  harder to spot warnings that we care about.
* You can set warnings as errors. We have an older scheme that does this for
  a subset of errors or you can simply specify `-Werror`, etc. as a part of
  `CFLAGS`. Configure is smart enough to strip it out when it runs configure
  checks. We build the C library with -Werror on GitHub, so you'll need to fix
  your warnings before creating a pull request.
* CMake has a developer mode that turns most these settings on.


### Branches

Please see `doc/branches-explained.md` for an explanation of our branching strategy.

For new small features, we have developers create feature branches in their own
repositories and then create pull requests into the canonical HDF5 repository.
For larger work, especially when the work will be done by multiple people, we
create feature branches named `feature/<feature>`. If work stops on a feature
branch, we rename it to `inactive/<feature>`.

If you create a feature branch in the canonical HDFGroup repository, please
create a `BRANCH.md` text file in the repository root and explain:

* The branch's purpose
* Contact info for someone who can tell us about the branch
* Clues about when the branch will be merged or can be considered for retirement

The purpose of this document is to avoid orphan branches with no clear
provenance.


### Pull requests

The process of creating a pull request is explained in `CONTRIBUTING.md`.


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

### Necessary software

In order to do development work on the HDF5 library, you will need to have
a few things available.

* A C99-compatible C compiler (MSVC counts). C11 is required to build the subfiling feature.
* Either CMake or the Autotools (Autoconf, Automake, libtool)
* Perl is needed to run some scripts, even on Windows
* A C++11-compatible compiler if you want to build the C++ wrappers
* A Fortran 2003-compatible compiler if you want to build the Fortran wrappers
* A Java 8-compatible compiler if you want to build the Java wrappers
* flex/lex and bison/yacc if you want to modify the high-level parsers
* A development version of zlib is necessary for zlib compression
* A development version of szip is necessary for szip compression
* An MPI-3 compatible MPI library must be installed for parallel HDF5 development
* clang-format is handy for formatting your code before submission to GitHub. The formatter will automatically update your PR if it's mis-formatted, though, so this isn't strictly necessary.
* codespell is useful to identify spelling issues before submission to GitHub. The codespell action won't automatically correct your code, but it will point out spelling errors, so this also isn't strictly necessary.

These are the requirements for working on the develop branch. Maintenance
branches may relax the required versions somewhat. For example, HDF5 1.12 and
earlier only require C++98.

Certain optional features may require additional libraries to be installed. You'll need curl and some S3 components installed to build the read-only S3 VFD, for example.

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
slightly between internal and public API calls.

Here's an example of an internal API call:

```c
/*
 * Function comments of dubious value
 */
herr_t
H5X_do_stuff(/*parameters*/)
{
	/* variables go here */
	void *foo = NULL;
	herr_t ret_value = SUCCEED;

	FUNC_ENTER_NOAPI(FAIL)

	HDassert(/*parameter check*/);

	if (H5X_other_call() < 0)
		HGOTO_ERROR(H5E_MAJ, H5E_MIN, FAIL, "badness");

done:
	if (ret_value < 0)
		/* do error cleanup */
	/* do regular cleanup stuff */

	FUNC_LEAVE_NOAPI(ret_value);
}
```

There are a couple of things to note here.

* Each function call has a header comment block. The information you'll find in
  most function comments is not particularly helpful. We're going to improve the
  format of this. 
* Most functions will return `herr_t` or `hid_t` ID. We try to avoid other
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
H5Xdo_api_stuff(/*parameters*/)
{
	/* variables go here */
	herr_t ret_value = SUCCEED;

	FUNC_ENTER_API(FAIL)
	H5TRACE3(/*stuff*/)

	if (/*parameter check*/)
		HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "badness");

	/* VOL setup */

	if (H5VL_call() < 0)
		HGOTO_ERROR(H5E_FOO, H5E_BAR, FAIL, "badness");

done:
	if (ret_value < 0)
		/* do error cleanup */
	/* do regular cleanup stuff */

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


### Public, private, package

HDF5 is divided into _packages_, which encapsulate related functionality. Each
has a prefix of the form `H5X(Y)`. An example is the dataset package, which has
the prefix `H5D`. Hopefully, you are familiar with this from the public API. In
addition to the public packages, we all know and love, there are many internal
packages that are not visible to the public via API calls, like `H5FL` (free
lists / memory pools) and `H5B2` (version 2 B-trees). There's also an `H5`
package that deals with very low-level things like library startup.

API calls, types, etc. in HDF5 have three levels of visibility. From most to
least visible, these are:

* Public
* Private
* Package

**Public** things are in the public API. They are usually found in `H5Xpublic.h`
header files. API calls are of the form `H5Xfoo()`, with no underscores between
the package name and the rest of the function name.

**Private** things are for use across the HDF5 library, and can be used outside the packages
that contain them. They collectively make up the "internal library API". API
calls are of the form `H5X_foo()` with one underscore between the package
name and the rest of the function name.

**Package** things are for use inside the package and the compiler will
complain if you include them outside of the package they belong to. They
collectively make up the "internal package API". API calls are of the form
`H5X__foo()` with *two* underscores between the package name and the rest of the
function name. The concept of "friend" packages exists and you can declare this
by defining `<package>_FRIEND` in a file. This will let you include the package
header from a package in a file that it is not a member of. Doing this is
strongly discouraged, though. Test functions are often declared in package
headers as they expose package internals and test programs can include
multiple package headers so they can check on internal package state.

Note that the underscore scheme is primarily for API calls and does not extend
to things like types and symbols. Another thing to keep in mind is that the
difference between package and private API calls can be somewhat arbitrary.
We're hoping to improve the coherence of the internal APIs via refactoring.


### Function enter and leave macros

Function enter and leave macros are added to almost all HDF5 API calls. This is
where we set up error handling (see below) and things like the thread-safety
global lock (in public API calls). There are different flavors depending on the
API call and it's very important that they are appropriate for the function they
mark up.

The various combinations you are most likely to encounter:

|Macro|Use|
|-----|---|
|`FUNC_ENTER_API`|Used when entering a public API call|
|`FUNC_ENTER_NOAPI`|Used when entering a private API call|
|`FUNC_ENTER_PACKAGE`|Used when entering a package API call|

There are also `_NO_INIT` flavors of some of these macros. These are usually
small utility functions that don't initialize the library, like
`H5is_library_threadsafe()`. They are uncommon.

You may also come across `_NO_FS` ("no function stack") flavors that don't push
themselves on the stack. These are rare.

For the most part, you will be using the `API`, `NOAPI`, and `PACKAGE` macros.

You may see other versions if you are working in a maintenance branch, like the
`STATIC` macros that we removed in 1.13. We've been working to reduce the
complexity and number of these macros and we don't always push that downstream
due to the scope of the changes involved. You should be able to figure out what
any new macros do based on what you've seen here, though.

### Error macros

Almost all HDF5 functions return an error code, usually -1 or some typedef'd
equivalent. Functions that return `void` should be avoided, even if the function
cannot fail. Instead, return an `herr_t` value and always return `SUCCEED`.

|Type|Error Value|
|----|-----------|
|`herr_t`|`FAIL`|
|any signed integer type|-1|
|`hid_t`|`H5I_INVALID_HID`|
|`htri_t`|`FAIL`|
|`haddr_t`|`HADDR_UNDEF`|
|pointer|`NULL`|

We've been trying to move away from using anything other than `herr_t` or `hid_t`
to return errors, as eliminating half of a variable's potential values just so
we can return a 'bad' value on errors seems unwise in a library that is
designed to scale.

`herr_t` is a typedef'd signed integer. In the library, we only define two
values for it: `SUCCEED` and `FAIL`, which are defined to 0 and -1, respectively,
in `H5private.h`. We do not export these values, so public API calls just note
that `herr_t` values will be negative on failure and non-negative on success.

Most of the error handling is performed using macros. The only extra setup you
will have to do is:

1. Create a variable named `ret_value` with the same type
as the return value for the function. If the type is `herr_t` it is frequently
set to `SUCCEED` and will be set to `FAIL` on errors. In most other cases,
the value is initialized to the 'bad' value and the function's code will set
`ret_value` to a 'good' value at some point, with errors setting it back to
the 'bad' value.

2. Create a done target (`done:`) just before you start your error cleanup.
This will be the point to which the error macros will jump.

We check for errors on almost all internal lines of C code that could putatively
fail. The general format is this:

```c
if (function_that_could_fail(foo, bar) < 0)
    HGOTO_ERROR(H5E_<major>, H5E_<minor>, <bad value>, "tell me about badness");
```

`HGOTO_ERROR` is one of a set of macros defined in `H5Eprivate.h`. This macro
pops an error on the error stack and sets the return value to `<bad value>`,
then jumps to the `done:` target.

Major and minor codes are a frequent cause of confusion. A full list of them
can be found in `H5err.txt`, which is processed into the actual error header
files at configure time by the `bin/make_err` script. The original intent was for major and minor error
codes to be strongly associated. i.e., a given minor code would *only* be used
with its associated major code. Unfortunately, this has not been the case in
practice, and the emitted text can appear nonsensical in error
stack dumps. Even worse, the major and minor error codes are used inconsistently
throughout the library, making interpreting them almost impossible for
external users. We hope to address this deficiency in the near future.

In the meantime, the following guidelines can be helpful:

1. Use `H5E_ARGS` as the major error category when parsing function parameters. The minor code will usually be `H5E_BADVALUE`, `H5E_BADRANGE`, or `H5E_BADTYPE`.
2. Otherwise use the same major code throughout the source file. There is almost a 1-1 correspondence between packages and major error codes.
3. Pick the minor code that seems to match the API call. You can grep through the library to find similar uses.
4. The string at the end of the `HGOTO_ERROR` macro is much more important, so make sure that is helpful

You will still sometimes see the major error code match the package of a failing
function call. We're trying to fix those as we come across them.

Since the `HGOTO_ERROR` macro jumps to the `done:` target, you can't use it
after the `done:` target without creating a loop. Instead, you'll need to use
the `HDONE_ERROR` macro, which will handle errors without jumping to the target.
Instead, processing will continue after pushing the error and setting the
return value, in the hopes that we can clean up as much as possible.

At the end of the function, the `FUNC_LEAVE` macro will return `ret_value`.

### Trace macros

These are automatically generated for public C library API calls by the
`bin/trace` script, which scans the source code, looking for functions of the
form `H5X(Y?)<whatever>()`, to which it will add or update the `H5TRACE` macros.

`H5TRACE` macros are only added to public C library API calls. They are NOT
added to the language wrappers, tools code, high-level library, etc.

You should never have to modify an `H5TRACE` macro. Either point `bin/trace` at
your source file or run `autogen.sh` (which runs `bin/trace` over the C files
in `src`). `bin/trace` is a Perl script, so you'll need to have that available.


### Memory - `H5MM` and `H5FL`

In the C library itself, we use `H5MM` and `H5FL` calls to allocate and free
memory instead of directly using the standard C library calls.

The `H5MM` package was originally designed so that it would be easy to swap in a
memory allocator of the developer's choosing. In practice, this has rarely
been a useful feature, and we are thinking about removing this scheme. In
the meantime, almost all memory allocations in the C library will use the
`H5MM` (or `H5FL`) package.

In the past, we added memory allocation sanity checking to the `H5MM` calls
which added heap canaries to memory allocations and performed sanity checking
and gathered statistics. These were turned on by default in debug builds
for many years. Unfortunately, there is interplay between library-allocated
and externally-allocated memory in the filter pipeline where the heap canaries
can easily get corrupted and cause problems. We also have some API calls that
return library-allocated memory to the user, which can cause problems if they
then use `free(3)` to free it. Given these problems, we now have the sanity
checks turned off by default in all build modes. You can turn them back on via
configure/CMake options, but it's normally easier to use external tools like
valgrind or the compiler's memory debugging options.

`H5FL` provides memory pools (*Free Lists*) that create a set of fixed-size allocations
of a certain type that the library will reuse as needed. They use `H5MM` calls
under the hood and can be useful when the library creates and frees a lot of
objects of that type. It's difficult to give a good guideline as to when to use
the `H5FL` calls and when to use the `H5MM` calls, but it's probably best to
lean towards `H5MM` unless you can identify a clear performance hit due to
memory cycling. Current library usage can be a good guide, but keep in mind that
the free lists are probably overused in the library. Another thing to keep in
mind is that the free lists can hide memory errors, like use-after-free. Some
developers always turn them off and you'll need to turn them off when running
memory checkers like valgrind.

Using free list calls differs little from using `H5MM` calls. There are
equivalents for `malloc(3)`, `calloc(3)`, and `free(3)`:

| C Library|`H5FL`|
|----------|------|
|`malloc`|`H5FL_MALLOC`|
|`calloc`|`H5FL_CALLOC`|
|`free`|`H5FL_FREE`|

Since free lists provide pre-allocated memory of a fixed size, you can't
reallocate free list memory and there's no `H5FL` `realloc(3)` equivalent.

You'll also need to add a setup macro to the top of the file. There are a few
flavors defined in `H5FLprivate.h`. Each creates global free list variables,
so there are flavors for extern, static, etc.

|Macro|Purpose|
|-----|-------|
|`H5FL_DEFINE`|Define a free list that will be used in several files|
|`H5FL_EXTERN`|Define a free list that was defined in another file|
|`H5FL_DEFINE_STATIC`|Define a free list that will only be used in this file|

You will also see `ARR`, `BLK`, `SEQ`, and `FAC` flavors of the macros. Their
use is beyond the scope of a guide for beginners.

## Testing

### Two macro schemes

The HDF5 C library is tested via a collection of small programs in the `test/`
directory. There are a few schemes in use:

- `testhdf5` - A larger, composite test program composed of several test files, most of which start with 't' (e.g., `tcoords.c`)
- Shell/Powershell scripts that test things like SWMR and flush/refresh behavior. These scripts run small sub-programs.
- Everything else. These are self-contained test programs that are built and run independently by the test harness.


The test programs do not use a standard test framework like cppunit, but instead
use HDF5-specific macros to set up the tests and report errors. There are two
sets of macros, one in `testhdf5.h` and another in `h5test.h`.
Originally, the `testhdf5` programs used the macros in `testhdf5.h` and everything
else used the macros in `h5test.h`, but over time the tests have evolved so that
all tests usually include both headers.

This is unfortunate, because it's very important to not mix up the "test framework
macros" in each scheme. The `testhdf5.h` macros register errors by setting global
variables and normally continue with the test when they encounter errors. The
`h5test.h` macros indicate errors by jumping to an error target and returning
a `FAIL` `herr_t` value. If you combine these two macro sets, you may
accidentally create tests that fail but do not register the failure.

We are aware that our testing scheme needs some work and we'll be working to
improve it over the next year or so.

The command-line tools are tested using a different scheme and are discussed elsewhere.

### `testhdf5.h`

The macros in this file are almost exclusively used in the `testhdf5` program.
They register errors by incrementing a global error count that is inspected at
the end of each test (not each test *function*, but each *test* - e.g., after
`test_file()` in `tfile.c` runs, but not when the individual functions it
calls run).

Test functions generally look like this:

```c
static void
test_something()
{
    /* Variables go here */
    int out = -1;
    herr_t ret;

    MESSAGE(5, ("Testing a thing\n"));

    ret = H5Xsome_api_call(&out);
    CHECK(ret, FAIL, "H5Xsome_api_call()");
    VERIFY(out, 6, "incorrect value for out");
}

```

The `MESSAGE` macro just dumps the name of what we're testing when we've the
verbosity cranked up. The confusingly-named `CHECK` and `VERIFY` macros are
members of a suite of check macros in `testhdf5.h`. `CHECK` macros check to
see if a variable is **NOT** a value, `VERIFY` macros check to see if a variable
**IS** a value. There are different flavors of macro to match different types
so be sure to use the right one to avoid compiler warnings and spurious errors.
Under the hood, these macros will emit error information and increment the
global error variables.

Tests are added to `testhdf5` in `testhdf5.c` using the `AddTest()` call. 
Each test will have a driver function, usually named something like `test_<thing>()`
that invokes the test functions in the file.
Most tests will cleanup their files using a `cleanup_<thing>()` call. If you are
deleting HDF5 files, you should use `H5Fdelete()` instead of `remove(3)` so
that files can be cleaned even when they use alternative VFDs or VOL connectors.
You'll also need to add prototypes for any new test driver or cleanup functions
to `testhdf5.h`.

Because these macros interact with global variables that are only used in the
testhdf5 program, they are useless anywhere else in the library. Even worse, it
will *look* like you are testing functionality, but errors will not be picked
up by the non-testhdf5 programs, hiding problems.

### `h5test.h`

These are the most commonly used macros and are used throughout the test code,
even in places that are not specifically tests. Unlike the scheme used in
the `testhdf5` program, these macros work more like the library, jumping to
an `error:` target on errors. There is no common `ret_value` variable, however.

Test functions will usually look like this:

```c
static herr_t
test_something()
{
    hid_t fapl_id = H5I_INVALID_HID;
    hid_t fid = H5I_INVALID_HID;
    char filename[1024];
    int *buf = NULL;

    TESTING("Testing some feature");

    if ((fapl_id = h5_fileaccess()) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* Many more calls here */

    PASSED();
    return SUCCEED;

error:

    HDfree(buf);

    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
}
```

Tests begin with a `TESTING` macro that emits some text (unlike the `testhdf5`
case, this is always dumped). Any errors will be handled by one of the
`TEST_ERROR` macros. For the most part, `TEST_ERROR` will suffice, but there
are others in `h5test.h` if you want to emit custom text, dump the HDF5 error
stack when it would not normally be triggered, etc.

Most tests will be set up to run with arbitrary VFDs. To do this, you set the
fapl ID using the `h5_fileaccess()` function, which will check the `HDF5_TEST_DRIVER`
environment variable and set the fapl's VFD accordingly. The `h5_fixname()`
call can then be used to get a VFD-appropriate filename for the `H5Fcreate()`,
etc. call.

In the `error` section, we clean up resources and return a 'bad' value, which
will usually either be `FAIL` or -1.

The `main()` function of each test program will usually start out by calling
`h5_reset()`, then run each test, incrementing an error count variable if a
test fails. The exit code and text will be set based on the error count.

### Scripts

If you need to fire off multiple programs to test a new feature, you may have
to do this via a script. These are normally named `test_<thing>.sh.in`. The
`.in` is because the scripts are often modified and copied during the configure
step. In the past, we have tried to stick to POSIX Bourne shell scripts, but
many scripts now require bash.

If you write a new test script, it is important to also add a PowerShell
equivalent for testing on Windows.

It's helpful to run any new shell scripts through `shellcheck`
(https://www.shellcheck.net/) to ensure that your scripts are free from
common problems.


### Parallel tests (in `testpar/`)

To be covered in a future update of this guide...

### Adding new tests

All new HDF5 library functionality (including bugfixes) should have a test.
Some rules of thumb:

- If you need to run multiple programs, you'll need to create a script and some test programs. Use the macros in `h5test.h` to handle errors in your test programs.
- If a suitable test program already exists (especially if your tests will be small), add your new tests to the existing file. 
- If you need to create a new test program, create one that uses the `h5test.h` macros.
- Avoid adding new tests to `testhdf5` or using the macros in `testhdf5.h`.

Don't forget that you'll need to add your test program or script to the lists in
both the CMake and Autotools test files (`CMakeLists.txt` and `Makefile.am` in
`test/` respectively). For simple tests, you just need to add your new test to
the list of tests.

All new tests **MUST** run under both the Autotools and CMake. Ideally, they
should also work on Windows, but we're willing to overlook this for things
that are unlikely to be useful on that platform.

## Documentation

We have moved the user guide and reference manual to Doxygen. All public API
calls and symbols should have Doxygen markup in the public header file. New major
features should be documented in the user guide. This Doxygen content is located in
the package's module header file (`H5Xmodule.h`). Language wrapper calls 
(C++/Fortran/Java) should also have Doxygen markup, which will be located with
the wrapper source code. Images and other common Doxygen files belong in the
`doxygen` directory.

Internal documentation for developer consumption is currently stored as Markdown
files in the `doc` directory. This may change in the future. Documentation that
helps understand the contents of a directory is often stored in a README.md
Markdown file in that directory.

Build and install documentation is stored in text files in `release_docs`. This
is admittedly not the best place for this. History files are also kept here.

## Command-Line Tools

The HDF5 command-line tools are written in C and built with the library by default.
The code is organized into a central tools library (in the `tools/lib` directory)
that includes some common functionality and the individual programs, each of which
has its own directory in `tools/src`. A few of the smaller tools are aggregated
into the `tools/src/misc` directory. Only h5diff has a parallel version at this
time and the parallel-specific functionality is in the `tools/src/h5diff/ph5diff_main.c` file.
Some h5diff functionality has also made its way into the tools library.
The tools code is not as well organized as the main library, so there's more opportunity
for refactoring here.

Also worth noting is that the command-line tools only use **public** HDF5 API
calls, even though they include `H5private.h` in order to take advantage of
the platform-independence scheme we use in the main library and some private
utility functions.

There are also a few tools in the high-level library. The gif2h5 and h52gif tools
are poorly-written and have known security issues, so they will soon be moved
to a separate repository, leaving h5watch as the only high-level command-line tool.

### Source code

The source code for the tools likes more like standard C code and uses its own
set of macros, which are defined in the tools library header files. There are
no `FUNC_ENTER` macros, you do not need to define a `ret_value` variable,
and the error macros are greatly simplified. Errors are usually handled by
a `TOOLS_ERROR` macro (or `TOOLS_GOTO_ERROR` if you need to jump to a `done:`
target to handle cleanup).
One area where the tools need a lot more work is in handling errors. The tools
code frequently ignores errors, often in functions that return `void`.

A "tools-only" consideration is the use of command-line arguments. We try to
be conservative about these, even though they really aren't in the "public API"
in the same way as API calls are. Additions and changes to the options will
probably result in some discussion.

### Tools tests

In most cases, a tool will be run against an input HDF5 file with a particular
set of command-line parameters, the exit code checked, and the output compared
with a standard output file. In some cases, errors are expected and standard
error files will be compared. These standard error files often contain HDF5
error stack dumps, which can cause spurious tool test "failures" when we
make changes to the main HDF5 C library.

Test files can be located in a few places in the `tools` directory tree.
Common input files that are used with multiple tools are kept in `tools/testfiles`.
Input files that are used with just one tool are located in `tools/test/<tool>/testfiles`.
Expected output files are located with their respective HDF5 files and end in `.txt`.
Expected error files are also located with their respective HDF5 files and end in `.err`.
h5dump files will usually have `.ddl` and `.xml` output files and there will
usually be `.<tool>` files that contain help output.
The test files are generated by programs with `gentest` in their name, though
we typically check the generated HDF5 files in instead of recreating them
as a part of running the tools tests.

The Autotools aggregate the tools tests in per-tool shell scripts in the
`tools/test/<tool>` directory. Each script starts with a few utility functions
that perform setup, compare files, clean output, etc. and the test commands
will appear at the end. CMake works similarly, but each test is set up in
the CMakeLists.txt and CMakeTests.cmake files in the same directory.

Adding a new test will usually involve:
- Adding a new function to the appropriate generator program to create a new HDF5 file
- Adding your new test to the CMake and Autotools test scripts, as described above
- Adding appropriate output and/or error files for comparison

You MUST add new tests to both the Autotools and CMake.
