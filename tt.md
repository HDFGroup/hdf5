# Welcome to HDF5 Development!

Welcome to the HDF5 development community! We're excited to have you on board.

The purpose of this document is to introduce new HDF5 developers to the source code and our development conventions. It's not a formal style guide, but instead a tour of the most common features and practices that might be new to someone who has never worked with the HDF5 source code before.

We encourage you to provide feedback. Corrections and suggestions for improvement should be handled via GitHub pull requests and issues.

---

## Prerequisites

Before you begin, you will need to have a few things available on your development machine.

* **A C99-compatible C compiler** (MSVC on Windows is supported). Note: The subfiling feature requires C11.
* **A build system:** Either **CMake** or the **Autotools** (Autoconf, Automake, libtool).
* **Perl:** Needed to run some build and test scripts, even on Windows.
* **clang-format:** Handy for formatting your code before submission. The CI system will automatically format your pull request if needed, so this isn't strictly required.
* **codespell:** Useful for identifying spelling issues before submission.

### Optional Components

Depending on which parts of the library you want to build or what features you want to enable, you may also need:
* A C++11-compatible compiler for the C++ wrappers.
* A Fortran 2003-compatible compiler for the Fortran wrappers.
* A Java 8-compatible compiler for the Java wrappers.
* `flex`/`lex` and `bison`/`yacc` if you want to modify the high-level parsers.
* Development versions of **zlib** and **szip** for compression support.
* An MPI-3 compatible MPI library for parallel HDF5 development.
* `curl` and other components for the read-only S3 VFD.

---

## Getting the Source Code

The HDF5 source code is hosted on GitHub. To get a copy, clone the repository using `git`:

```bash
git clone [https://github.com/HDFGroup/hdf5.git](https://github.com/HDFGroup/hdf5.git)
cd hdf5
````

-----

## Building the Library for Development

While you don't need special configuration settings to build the library, there are several options that are very useful for developers. We recommend starting with a debug build, as this enables many helpful checks within the library.

### Basic CMake Build (Recommended)

CMake is the preferred build system, especially on Windows. A typical developer build looks like this:

1.  **Create a build directory:**
    ```bash
    mkdir build && cd build
    ```
2.  **Configure the build:** The `HDF5_ENABLE_DEVELOPER_MODE` option conveniently turns on most settings useful for development (debug symbols, warnings as errors, etc.).
    ```bash
    cmake -G "Unix Makefiles" -DHDF5_ENABLE_DEVELOPER_MODE=ON ..
    ```
3.  **Build the library:**
    ```bash
    make
    ```

### Basic Autotools Build

1.  **Generate the configure script:**
    ```bash
    ./autogen.sh
    ```
2.  **Configure the build:** `--enable-debug` is the primary flag for a development build.
    ```bash
    ./configure --enable-debug
    ```
3.  **Build the library:**
    ```bash
    make
    ```

### Useful Build Tips for Developers

  * **Memory Checking:** If you are using tools like Valgrind to find memory issues, you must disable the library's internal memory pools (free lists). This is done with the `--enable-using-memchecker` configure option (or the equivalent CMake flag). Some developers build with this on all the time, as memory recycling can hide problems like use-after-free.
  * **Developer Warnings:** You can enable extra warnings via `--enable-developer-warnings`. These warnings generate a lot of noise but can occasionally be useful.
  * **Warnings as Errors:** The CI system on GitHub builds the C library with `-Werror`, so you will need to fix all compiler warnings before creating a pull request.

-----

## A Brief Tour of the Source Code

Here's a quick guide to where you can find things in the source tree.

  * `bin/`: Scripts used for building the software and miscellaneous tools.
  * `c++/`: Source, tests, and examples for the C++ language wrapper.
  * `config/`: Configuration files for both Autotools and CMake.
  * `docs/`: Miscellaneous developer documents, mostly in markdown format.
  * `doxygen/`: Doxygen build files and top-level documentation content.
  * `examples/`: C library examples.
  * `fortran/`: Source, tests, and examples for the Fortran language wrapper.
  * `hl/`: Source, tests, and examples for the high-level library.
  * `java/`: Source, tests, and examples for the JNI/Java language wrapper.
  * `m4/`: m4 build scripts used by the Autotools.
  * `release_docs/`: Install instructions and release notes.
  * `src/`: Source code for the main C library.
  * `test/`: C library test code.
  * `testpar/`: Parallel C library test code.
  * `tools/`: Command-line tools (h5dump, h5repack, etc.).
  * `utils/`: Small utility programs.

-----

## HDF5 Development Conventions

### Anatomy of an HDF5 API Call

HDF5 API calls have a uniform structure for function entry/exit and error handling. We stick to this boilerplate for all C functions.

Here's an example of an **internal** API call:

```c
/*
 * Function comments
 */
herr_t
H5X_do_stuff(/*parameters*/)
{
    /* 1. Variables declared at top */
    void *foo = NULL;
    herr_t ret_value = SUCCEED; /* 2. Return value variable */

    FUNC_ENTER_NOAPI(FAIL) /* 3. Function entry macro */

    HDassert(/*parameter check*/);

    /* 4. Check for errors and goto done */
    if (H5X_other_call() < 0)
        HGOTO_ERROR(H5E_MAJ, H5E_MIN, FAIL, "An error occurred");

done: /* 5. Target for error jumps */
    if (ret_value < 0)
        /* do error cleanup */
    /* do regular cleanup */

    FUNC_LEAVE_NOAPI(ret_value); /* 6. Function leave macro */
}
```

A **public** API call is very similar but uses `FUNC_ENTER_API`, performs more rigorous parameter checking, includes an `H5TRACE` macro for API tracing, and may include VOL setup.

### Code Visibility: Public, Private, and Package

HDF5 code is divided into *packages* which encapsulate related functionality (e.g., `H5D` for datasets). Within the library, functions and types have three levels of visibility, identified by a naming convention for function calls:

  * **Public:** Exposed in the public API for users. Found in `H5Xpublic.h` headers.
      * **Format:** `H5Xfoo()` (e.g., `H5Dcreate`)
  * **Private:** For use anywhere across the HDF5 library; our "internal library API".
      * **Format:** `H5X_foo()` (one underscore, e.g., `H5D_create`)
  * **Package:** For use only inside the package where they are defined.
      * **Format:** `H5X__foo()` (two underscores, e.g., `H5D__create`)

### Function enter/leave and Error Handling Macros

Almost all functions return an error code (`herr_t` or `hid_t`). The `FUNC_ENTER_*` macro sets up the error handling stack for the function. On an error, the `HGOTO_ERROR` macro pushes an error onto the stack, sets the `ret_value` variable, and jumps to the `done:` label for cleanup. The `FUNC_LEAVE_*` macro then returns `ret_value`.

This structure ensures that resources are cleaned up correctly, whether the function succeeds or fails. When adding new code, always check the return value of any function that could fail.

### Platform Independence

HDF5 was created when the Unix world was more fragmented and C99 was uncommon. To handle platform differences, we use a compatibility layer.

  * Most standard C and POSIX calls are prefixed with `HD` (e.g., `HDmalloc`, `HDopen`).
  * The `H5private.h` and `H5win32defs.h` headers map these `HD` calls to the correct platform-specific functions.

We are slowly modernizing this layer as C99 and POSIX have become widespread.

### Memory Management: `H5MM` and `H5FL`

Instead of `malloc` and `free`, the C library uses internal wrappers for memory management.

  * **`H5MM`:** A general-purpose memory management package. In practice, this almost always maps directly to standard C library calls. Use this for most allocations.
  * **`H5FL`:** Provides memory pools (*Free Lists*) for fixed-size allocations that are frequently created and destroyed. This can improve performance by avoiding calls to `malloc`/`free`, but it can also hide memory errors. Lean towards using `H5MM` unless you identify a clear performance reason to use a free list.

-----

## Contributing to HDF5

### Branches

Our branching strategy is explained in `docs/branches-explained.md`.

  * **Small features** are developed in forks of the HDF5 repository.
  * **Larger, collaborative work** uses feature branches in the main repository, named `feature/<feature>`.

If you create a feature branch in the canonical HDFGroup repository, please add a `BRANCH.md` file to the repository root explaining the branch's purpose and contact info.

### Pull Requests

The process of creating a pull request is explained in `CONTRIBUTING.md`. All new functionality and bugfixes should be submitted as pull requests and must include tests.

-----

## Testing

The HDF5 C library is tested by a collection of small programs in the `test/` and `testpar/` directories. We do not use a standard framework like CppUnit, but instead use HDF5-specific macros. Unfortunately, there are two different sets of macros, and it is important not to mix them.

  * **`h5test.h`:** The modern, preferred testing scheme. Macros like `TESTING("Some feature")` and `TEST_ERROR` are used. On failure, the test function jumps to an `error:` label for cleanup and returns `FAIL`.
  * **`testhdf5.h`:** A legacy scheme used exclusively by the large `testhdf5` program. It uses global variables to track errors and should be avoided for new tests.

### Adding New Tests

All new functionality and bugfixes **must** have a test.

1.  If a suitable test file already exists, add your new tests there.
2.  If you create a new test program, use the macros in `h5test.h`.
3.  Avoid adding new tests to the `testhdf5` program.
4.  Remember to add your new test program or script to the `CMakeLists.txt` and `Makefile.am` files in the `test/` directory. All new tests must run under both CMake and Autotools.

-----

## Command-Line Tools

The HDF5 command-line tools (e.g., `h5dump`, `h5diff`) are located in the `tools/` directory. They are written in C and use only the **public** HDF5 API. The code is organized into a central tools library (`tools/lib`) and individual directories for each tool.

The tools use a simplified error-handling scheme compared to the main library, defined in the tools library header files.

-----

## Documentation

Our documentation is generated using Doxygen.

  * **Reference Manual:** All public API calls must have Doxygen markup in the public header files (`H5Xpublic.h`).
  * **User Guide:** New major features should be documented in the user guide. This content is located in the package's module header file (`H5Xmodule.h`).
  * **Developer Docs:** Internal documentation for developers is stored as Markdown files in the `docs/` directory.

-----

## Getting Help

If you have questions or get stuck, the HDF5 community is here to help.

  * **The HDF Forum:** This is the best place to ask questions about HDF5 usage and development. You can find it on the HDF Group website.
  * **GitHub Issues:** For bug reports or specific feature requests, please open an issue on our GitHub repository.

<!-- end list -->

```
