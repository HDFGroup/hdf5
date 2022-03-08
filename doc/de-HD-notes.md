# De-`HD`-ification notes

This document records the steps already taken to remove the `HD`
prefix from C/POSIX library functions that are universally available
and standards-compliant.

# Step one

As a first step, a plain C99 function call replaced the use of the
equivalent `HD` wrapper in almost every instance, and the disused `HD`
names were "poisoned" to protect against their reuse in the future.

Since 2021, every supported platform for the HDF5 library, tests, and
tools has a C compiler and standard library with good conformance to the
ISO C99 standard.  C99 support is uniform enough that ordinarily it is
possible to use every C99 function name directly instead of employing a
wrapper macro to smooth over differences between, say, Windows and Linux.

One C99 function that retains its `HD` prefix is `fseek`, since it is
necessary on Linux and Windows to use different platform functions to
perform a 64-bit seek.  Otherwise, the use of the `HD`-prefixed C99
functions have been phased out.

# Scripts

Under `bin/` at the top-level HDF5 library directory, there is a
Bourne-shell script, `dehd-c99`, that rewrites all `HD`-prefixed C99
function names with plain C99 function names in every source file except
for a handful of header files that require special treatment because they
actually define the `HD` wrappers.  `dehd-c99` refreshes a header file,
`src/H5poison.h`, that "poisons" the `HD`-prefixed C99 function names by
`#define`ing them to names that the compiler will not find declarations
for, and the linker will not find objects for.

# `HD` implementation oddities

Most of the `HD` wrapper macros for C99 functions just passed
their arguments directly to the C99 function, e.g.,

```C
#define HDcos(x) cos(x)
```

A few wrapper macros clamped a parameter to an integer range or cast
a parameter to a different type before passing it to a C99 function.
In every instance, the casts and clamping was not well justified so it
was removed.

## memset

A cast was removed.

Comments said that on MSVC, the `memset` destination had to be cast to
`(void *)` to avoid an MSVC warning:

```C
#define HDmemset(X, C, Z)    memset((void *)(X), C, Z) /* Cast avoids MSVC warning */
```

A web search shows that `C4090` is probably that warning, and a
long-standing bug in Visual Studio causes it to generate the warning.
Let us see how many times warning C4090 actually occurs without the cast,
and see if we can manage the warnings by modifying a handful of callsites
instead of using a macro.

## memmove

A cast was removed.

According to an old comment, the optimizer for some compiler emitted
incorrect DEC Alpha code when the source and/or destination for
`memmove` was not aligned.  That is why the `HDmemmove` wrapper for
`memmove` cast the `memmove` destination and source to `char *` and
`const char *`, respectively.  It appears that the casts may have
worked around a Linux bug affecting DEC Alpha [that was fixed in
2000](https://lkml.iu.edu/hypermail/linux/kernel/0012.2/0712.html).
That bug should not affect any supported system any longer.

## setvbuf

Comments said that on Windows, `setvbuf` would not accept a buffer size
less than 2.  `HDsetvbuf` clamped the buffer size at 2 or greater.  The
the library, tests, and tools now pass `BUFSIZ` instead of
`0` as the buffer size.  C defines `BUFSIZ`, so it should be a valid
buffer size on any platform.

## isalnum, isalpha, isdigit, et cetera

Each `<ctype.h>` wrapper macro---e.g., `HDisalpha`---cast its parameter
to `int` in order to quiet a warning on "Solaris."  Casting a `char` or
`signed char` to `int` may suppress warnings, but it does *not* bring
all negative values into the domain of the `ctype.h` functions.  Now,
HDF5 uses the portable idiom at each callsite where that is appropriate:
`is...(c)` becomes `is...((int)(unsigned char)(c))`.
