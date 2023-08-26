# CMake Scripts <!-- omit in toc -->

[![pipeline status](https://git.stabletec.com/other/cmake-scripts/badges/master/pipeline.svg)](https://git.stabletec.com/other/cmake-scripts/commits/master)
[![license](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](https://git.stabletec.com/other/cmake-scripts/blob/master/LICENSE)

This is a collection of quite useful scripts that expand the possibilities for building software with CMake, by making some things easier and otherwise adding new build types

- [C++ Standards `c++-standards.cmake`](#c-standards-c-standardscmake)
- [Sanitizer Builds `sanitizers.cmake`](#sanitizer-builds-sanitizerscmake)
- [Code Coverage `code-coverage.cmake`](#code-coverage-code-coveragecmake)
  - [Added Targets](#added-targets)
  - [Usage](#usage)
    - [Example 1 - All targets instrumented](#example-1---all-targets-instrumented)
      - [1a - Via global command](#1a---via-global-command)
      - [1b - Via target commands](#1b---via-target-commands)
    - [Example 2: Target instrumented, but with regex pattern of files to be excluded from report](#example-2-target-instrumented-but-with-regex-pattern-of-files-to-be-excluded-from-report)
    - [Example 3: Target added to the 'ccov' and 'ccov-all' targets](#example-3-target-added-to-the-ccov-and-ccov-all-targets)
- [Compiler Options `compiler-options.cmake`](#compiler-options-compiler-optionscmake)
- [Dependency Graph `dependency-graph.cmake`](#dependency-graph-dependency-graphcmake)
  - [Required Arguments](#required-arguments)
    - [OUTPUT_TYPE *STR*](#output_type-str)
  - [Optional Arguments](#optional-arguments)
    - [ADD_TO_DEP_GRAPH](#add_to_dep_graph)
    - [TARGET_NAME *STR*](#target_name-str)
    - [OUTPUT_DIR *STR*](#output_dir-str)
- [Doxygen `doxygen.cmake`](#doxygen-doxygencmake)
  - [Optional Arguments](#optional-arguments-1)
    - [ADD_TO_DOC](#add_to_doc)
    - [INSTALLABLE](#installable)
    - [PROCESS_DOXYFILE](#process_doxyfile)
    - [TARGET_NAME *STR*](#target_name-str-1)
    - [OUTPUT_DIR *STR*](#output_dir-str-1)
    - [INSTALL_PATH *STR*](#install_path-str)
    - [DOXYFILE_PATH *STR*](#doxyfile_path-str)
- [Prepare the Catch Test Framework `prepare_catch.cmake`](#prepare-the-catch-test-framework-prepare_catchcmake)
  - [Optional Arguments](#optional-arguments-2)
    - [COMPILED_CATCH](#compiled_catch)
    - [CATCH1](#catch1)
    - [CLONE](#clone)
- [Tools `tools.cmake`](#tools-toolscmake)
  - [clang-tidy](#clang-tidy)
  - [include-what-you-use](#include-what-you-use)
  - [cppcheck](#cppcheck)
- [Formatting `formatting.cmake`](#formatting-formattingcmake)
  - [clang-format](#clang-format)
  - [cmake-format](#cmake-format)

## C++ Standards [`c++-standards.cmake`](c++-standards.cmake)

Using the functions `cxx_11()`, `cxx_14()`, `cxx_17()` or `cxx_20()` this adds the appropriate flags for both unix and MSVC compilers, even for those before 3.11 with improper support.

These obviously force the standard to be required, and also disables compiler-specific extensions, ie `--std=gnu++11`. This helps to prevent fragmenting the code base with items not available elsewhere, adhering to the agreed C++ standards only.

## Sanitizer Builds [`sanitizers.cmake`](sanitizers.cmake)

Sanitizers are tools that perform checks during a program's runtime and return issues, and as such, along with unit testing, code coverage and static analysis, are another tool to add to the programmer's toolbox. And, of course, like the previous tools, they are simple to add to any project using CMake, allowing any project and developer to quickly and easily use them.

A quick rundown of the tools available, and what they do:
- [LeakSanitizer](https://clang.llvm.org/docs/LeakSanitizer.html) detects memory leaks, or issues where memory is allocated and never deallocated, causing programs to slowly consume more and more memory, eventually leading to a crash.
- [AddressSanitizer](https://clang.llvm.org/docs/AddressSanitizer.html) is a fast memory error detector. It is useful for detecting most issues dealing with memory, such as:
    - Out of bounds accesses to heap, stack, global
    - Use after free
    - Use after return
    - Use after scope
    - Double-free, invalid free
    - Memory leaks (using LeakSanitizer)
- [ThreadSanitizer](https://clang.llvm.org/docs/ThreadSanitizer.html) detects data races for multi-threaded code.
- [UndefinedBehaviourSanitizer](https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html) detects the use of various features of C/C++ that are explicitly listed as resulting in undefined behaviour. Most notably:
    - Using misaligned or null pointer.
    - Signed integer overflow
    - Conversion to, from, or between floating-point types which would overflow the destination
    - Division by zero
    - Unreachable code
- [MemorySanitizer](https://clang.llvm.org/docs/MemorySanitizer.html) detects uninitialized reads.

These are used by declaring the `USE_SANITIZER` CMake variable as one of:
- Address
- Memory
- MemoryWithOrigins
- Undefined
- Thread
- Address;Undefined
- Undefined;Address
- Leak

## Code Coverage [`code-coverage.cmake`](code-coverage.cmake)

![Code Coverage Examples](img/code-cov.png)

> In computer science, test coverage is a measure used to describe the degree to which the source code of a program is executed when a particular test suite runs. A program with high test coverage, measured as a percentage, has had more of its source code executed during testing, which suggests it has a lower chance of containing undetected software bugs compared to a program with low test coverage. Many different metrics can be used to calculate test coverage; some of the most basic are the percentage of program subroutines and the percentage of program statements called during execution of the test suite. 
>
> [Wikipedia, Code Coverage](https://en.wikipedia.org/wiki/Code_coverage)

Code coverage is the detailing of, during the execution of a binary, which regions, functions, or lines of code are *actually* executed. This can be used in a number of ways, from figuring out areas that automated testing is lacking or not touching, to giving a user an instrumented binary to determine which areas of code are used most/least to determine which areas to focus on. Although this does come with the caveat that coverage is no guarantee of good testing, just of what code has been.

Coverage here is supported on both GCC and Clang. GCC requires the `lcov` program, and Clang requires `llvm-cov` and `llvm-profdata`, often provided with the llvm toolchain.

To enable, turn on the `CODE_COVERAGE` variable.

### Added Targets

- GCOV/LCOV:
    - ccov : Generates HTML code coverage report for every target added with 'AUTO' parameter.
    - ccov-${TARNGET_NAME} : Generates HTML code coverage report for the associated named target.
    - ccov-all : Generates HTML code coverage report, merging every target added with 'ALL' parameter into a single detailed report.
    - ccov-all-capture : Generates an all-merged.info file, for use with coverage dashboards (e.g. codecov.io, coveralls).
- LLVM-COV:
    - ccov : Generates HTML code coverage report for every target added with 'AUTO' parameter.
    - ccov-report : Generates HTML code coverage report for every target added with 'AUTO' parameter.
    - ccov-${TARGET_NAME} : Generates HTML code coverage report.
    - ccov-rpt-${TARGET_NAME} : Prints to command line summary per-file coverage information.
    - ccov-show-${TARGET_NAME} : Prints to command line detailed per-line coverage information.
    - ccov-all : Generates HTML code coverage report, merging every target added with 'ALL' parameter into a single detailed report.
    - ccov-all-report : Prints summary per-file coverage information for every target added with ALL' parameter to the command line.

### Usage

To enable any code coverage instrumentation/targets, the single CMake option of `CODE_COVERAGE` needs to be set to 'ON', either by GUI, ccmake, or on the command line ie `-DCODE_COVERAGE=ON`.

From this point, there are two primary methods for adding instrumentation to targets:
1. A blanket instrumentation by calling `add_code_coverage()`, where all targets in that directory and all subdirectories are automatically instrumented.
2. Per-target instrumentation by calling `target_code_coverage(<TARGET_NAME>)`, where the target is given and thus only that target is instrumented. This applies to both libraries and executables.

To add coverage targets, such as calling `make ccov` to generate the actual coverage information for perusal or consumption, call `target_code_coverage(<TARGET_NAME>)` on an *executable* target.

**NOTE:** For more options, please check the actual [`code-coverage.cmake`](code-coverage.cmake) file.

#### Example 1 - All targets instrumented

In this case, the coverage information reported will will be that of the `theLib` library target and `theExe` executable.

##### 1a - Via global command

```
add_code_coverage() # Adds instrumentation to all targets

add_library(theLib lib.cpp)

add_executable(theExe main.cpp)
target_link_libraries(theExe PRIVATE theLib)
target_code_coverage(theExe) # As an executable target, adds the 'ccov-theExe' target (instrumentation already added via global anyways) for generating code coverage reports.
```

##### 1b - Via target commands

```
add_library(theLib lib.cpp)
target_code_coverage(theLib) # As a library target, adds coverage instrumentation but no targets.

add_executable(theExe main.cpp)
target_link_libraries(theExe PRIVATE theLib)
target_code_coverage(theExe) # As an executable target, adds the 'ccov-theExe' target and instrumentation for generating code coverage reports.
```

#### Example 2: Target instrumented, but with regex pattern of files to be excluded from report

```
add_executable(theExe main.cpp non_covered.cpp)
target_code_coverage(theExe EXCLUDE non_covered.cpp) # As an executable target, the reports will exclude the non-covered.cpp file.
```

#### Example 3: Target added to the 'ccov' and 'ccov-all' targets

```
add_code_coverage_all_targets(EXCLUDE test/*) # Adds the 'ccov-all' target set and sets it to exclude all files in test/ folders.

add_executable(theExe main.cpp non_covered.cpp)
target_code_coverage(theExe AUTO ALL EXCLUDE non_covered.cpp test/*) # As an executable target, adds to the 'ccov' and ccov-all' targets, and the reports will exclude the non-covered.cpp file, and any files in a test/ folder.
```

## Compiler Options [`compiler-options.cmake`](compiler-options.cmake)

Allows for easy use of some pre-made compiler options for the major compilers.

Using `-DENABLE_ALL_WARNINGS=ON` will enable almost all of the warnings available for a compiler:

| Compiler | Options       |
| :------- | :------------ |
| MSVC     | /W4           |
| GCC      | -Wall -Wextra |
| Clang    | -Wall -Wextra |

Using `-DENABLE_EFFECTIVE_CXX=ON` adds the `-Weffc++` for both GCC and clang.

Using `-DGENERATE_DEPENDENCY_DATA=ON` generates `.d` files along with regular object files on a per-source file basis on GCC/Clang compilers. These files contains the list of all header files used during compilation of that compilation unit.

## Dependency Graph [`dependency-graph.cmake`](dependency-graph.cmake)

CMake, with the dot application available, will build a visual representation of the library/executable dependencies, like so:
![Dependency Graph](img/dp-graph.png)

### Required Arguments

#### OUTPUT_TYPE *STR*
The type of output of `dot` to produce. Can be whatever `dot` itself supports (eg. png, ps, pdf).

### Optional Arguments

#### ADD_TO_DEP_GRAPH
If specified, add this generated target to be a dependency of the more general `dep-graph` target.

#### TARGET_NAME *STR*
The name to give the doc target. (Default: doc-${PROJECT_NAME})

#### OUTPUT_DIR *STR*
The directory to place the generated output

## Doxygen [`doxygen.cmake`](doxygen.cmake)

Builds doxygen documentation with a default 'Doxyfile.in' or with a specified one, and can make the results installable (under the `doc` install target)

This can only be used once per project, as each target generated is as `doc-${PROJECT_NAME}` unless TARGET_NAME is specified.

### Optional Arguments

#### ADD_TO_DOC
If specified, adds this generated target to be a dependency of the more general `doc` target.

#### INSTALLABLE
Adds the generated documentation to the generic `install` target, under the `documentation` installation group.

#### PROCESS_DOXYFILE
If set, then will process the found Doxyfile through the CMAKE `configure_file` function for macro replacements before using it. (@ONLY)

#### TARGET_NAME *STR*
The name to give the doc target. (Default: doc-${PROJECT_NAME})

#### OUTPUT_DIR *STR*
The directory to place the generated output. (Default: ${CMAKE_CURRENT_BINARY_DIR}/doc)

#### INSTALL_PATH *STR*
The path to install the documenttation under. (if not specified, defaults to 'share/${PROJECT_NAME})

#### DOXYFILE_PATH *STR*
The given doxygen file to use/process. (Defaults to'${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile')

## Prepare the Catch Test Framework [`prepare_catch.cmake`](prepare_catch.cmake)

The included `prepare_catch` function contained within attempts to add the infrastructure necessary for automatically adding C/C++ tests using the Catch2 library, including either an interface or pre-compiled 'catch' target library.

It first attempts to find the header on the local machine, and failing that, clones the single header variant for use. It does make the determination between pre-C++11 and will use Catch1.X rather than Catch2 (when cloned), automatically or forced.. Adds a subdirectory of tests/ if it exists from the macro's calling location.

### Optional Arguments

#### COMPILED_CATCH
If this option is specified, then generates the 'catch' target as a library with catch already pre-compiled as part of the library. Otherwise acts just an interface library for the header location.

#### CATCH1
Force the use of Catch1.X, rather than auto-detecting the C++ version in use.

#### CLONE
Force cloning of Catch, rather than attempting to use a locally-found variant.

## Tools [`tools.cmake`](tools.cmake)

### clang-tidy

> clang-tidy is a clang-based C++ “linter” tool. Its purpose is to provide an extensible framework for diagnosing and fixing typical programming errors, like style violations, interface misuse, or bugs that can be deduced via static analysis. clang-tidy is modular and provides a convenient interface for writing new checks.
>
> [clang-tidy page](https://clang.llvm.org/extra/clang-tidy/)

When detected, [clang-tidy](https://clang.llvm.org/extra/clang-tidy/) can be enabled by using the option of `-DCLANG_TIDY=ON`, as it is disabled by default.

To use, add the `clang_tidy()` function, with the arguments being the options to pass to the clang tidy program, such as '-checks=*'.

### include-what-you-use

This tool helps to organize headers for all files encompass all items being used in that file, without accidentally relying upon headers deep down a chain of other headers. This is disabled by default, and can be enabled via have the program installed and adding `-DIWYU=ON`.

To use, add the `include_what_you_use()` function, with the arguments being the options to pass to the program.

### cppcheck

This tool is another static analyzer in the vein of clang-tidy, which focuses on having no false positives. This is by default disabled, and can be enabled via have the program installed and adding `-DCPPCHECK=ON`.

To use, add the `cppcheck()` function, with the arguments being the options to pass to the program.

## Formatting [`formatting.cmake`](formatting.cmake)

### clang-format

Allows to automatically perform code formatting using the clang-format program, by calling an easy-to-use target ala `make format`. It requires a target name, and the list of files to format. As well, if the target name is the name of another target, then all files associated with that target will be added, and the target name changed to be `format_<TARGET>`. As well, any targets otherwise listed with the files will also have their files imported for formatting.

```
file(GLOB_RECURSE ALL_CODE_FILES
    ${PROJECT_SOURCE_DIR}/src/*.[ch]pp
    ${PROJECT_SOURCE_DIR}/src/*.[ch]
    ${PROJECT_SOURCE_DIR}/include/*.[h]pp
    ${PROJECT_SOURCE_DIR}/include/*.[h]
    ${PROJECT_SOURCE_DIR}/example/*.[ch]pp
    ${PROJECT_SOURCE_DIR}/example/*.[ch]
)

clang_format(TARGET_NAME ${ALL_CODE_FILES})
```

### cmake-format

Similar to the clang-format above, creates a target `cmake-format` when the `cmake_format(<FILES>)` function is defined in CMake scripts, and any <FILES> passed in will be formatted by the cmake-format program, if it is found.

```
file(GLOB_RECURSE CMAKE_FILES
    CMakeLists.txt
)

cmake_format(TARGET_NAME ${CMAKE_FILES})
```
