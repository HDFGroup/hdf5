# Scripts in `bin` and their purpose

|Program|Purpose|
|-------|-------|
|`checkapi`|Checks if public API calls are used in internal functions|
|`chkcopyright`|Checks if files have appropriate copyright statements|
|`debug-ohdr`|Examines debug output from `H5O_open/close` to look for open objects|
|`format_source`|Runs `clang-format` over the source files, applying our rules|
|`genparser`|Creates the flex/bison-based parser files in the high-level library|
|`h5cc.in`|Input file from which h5cc is created|
|`h5redeploy.in`|Input file from which h5redeploy is created|
|`h5vers`|Updates the library version number|
|`make_err`|Generates the H5E header files|
|`make_vers`|Generates H5version.h|
|`make_overflow`|Generates H5overflow.h|
|`output_filter`|Used in the tools test code to strip extraneous output before we diff files|
|`runbkprog`|Used by CMake to run test programs in the background|
|`trace`|Updates `H5ARG_TRACE` macros in H5ES\_insert() calls|
|`warnhist`|Generates compiler warning statistics for gcc/clang when fed output of make|

## TODO

* chkcopyright is currently semi-broken as it doesn't handle the full variety of copyright headers we need. We're leaving it in place, though, in the hopes that someone will update it in the future.
* Extending warnhist to better understand the output of additional compilers/languages would be nice.
