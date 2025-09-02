# HDF5 Development Guide

Welcome to the HDF5 development community! This comprehensive guide covers everything you need to know about contributing to HDF5, from getting started to submitting your changes.

> [!NOTE]
> No contribution can be accepted unless the contributor agrees to the HDF Group's software license terms, which can be found in the LICENSE file located in the top source directory of every branch.

## Table of Contents

- [Getting Started](#getting-started)
- [Prerequisites](#prerequisites)
- [Getting the Source Code](#getting-the-source-code)
- [Building for Development](#building-for-development)
- [Source Code Overview](#source-code-overview)
- [Development Conventions](#development-conventions)
- [Contributing Changes](#contributing-changes)
- [Testing](#testing)
- [Documentation](#documentation)
- [Getting Help](#getting-help)

---

## Getting Started

If you're new to Git and GitHub, please review the [GitHub tutorial](https://guides.github.com/activities/hello-world/) (takes about 10 minutes).

The HDF Group welcomes all contributions - from fixing typos to adding major features. We're committed to making the contribution process enjoyable and straightforward.

---

## Prerequisites

Before you begin, ensure your development machine has:

### Required Tools
* **A C99-compatible C compiler** (MSVC on Windows is supported). Note: The subfiling feature requires C11.
* **A build system:** Either **CMake** (recommended) or the **Autotools** (Autoconf, Automake, libtool).
* **Perl:** Needed to run build and test scripts, even on Windows.
* **Git:** For version control.

### Recommended Tools
* **clang-format:** For code formatting. The CI system will automatically format pull requests if needed.
* **codespell:** For identifying spelling issues before submission.

### Optional Components
Depending on which features you want to build or enable:
* A C++11-compatible compiler for the C++ wrappers.
* A Fortran 2003-compatible compiler for the Fortran wrappers.
* A Java 8-compatible compiler for the Java wrappers.
* `flex`/`lex` and `bison`/`yacc` if you want to modify the high-level parsers.
* Development versions of **zlib** and **szip** for compression support.
* An MPI-3 compatible MPI library for parallel HDF5 development.
* `curl` and other components for the read-only S3 VFD.

---

## Getting the Source Code

The HDF5 source code is hosted on GitHub:

```bash
git clone https://github.com/HDFGroup/hdf5.git
cd hdf5
```

---

## Building for Development

### Basic CMake Build (Recommended)

CMake is the preferred build system, especially on Windows:

1. **Create a build directory:**
   ```bash
   mkdir build && cd build
   ```

2. **Configure the build:**
   ```bash
   cmake -G "Unix Makefiles" -DHDF5_ENABLE_DEVELOPER_MODE=ON ..
   ```
   The `HDF5_ENABLE_DEVELOPER_MODE` option enables debug symbols, warnings as errors, and other developer-friendly settings.

3. **Build the library:**
   ```bash
   make
   ```

### Basic Autotools Build

1. **Generate the configure script:**
   ```bash
   ./autogen.sh
   ```

2. **Configure the build:**
   ```bash
   ./configure --enable-debug
   ```

3. **Build the library:**
   ```bash
   make
   ```

### Developer Build Tips

* **Memory Checking:** Use `--enable-using-memchecker` (or equivalent CMake flag) when using tools like Valgrind. This disables internal memory pools that can hide memory issues.
* **Developer Warnings:** Enable extra warnings with `--enable-developer-warnings` (generates significant output but can be useful).
* **Warnings as Errors:** The CI system builds with `-Werror`, so fix all compiler warnings before submitting pull requests.

---

## Source Code Overview

Here's where to find things in the source tree:

* **`src/`**: Main C library source code
* **`test/`**: C library test code
* **`testpar/`**: Parallel C library test code
* **`tools/`**: Command-line tools (h5dump, h5repack, etc.)
* **`examples/`**: C library examples
* **`hl/`**: High-level library source, tests, and examples
* **`c++/`**: C++ language wrapper
* **`fortran/`**: Fortran language wrapper
* **`java/`**: JNI/Java language wrapper
* **`bin/`**: Build scripts and miscellaneous tools
* **`config/`**: Configuration files for Autotools and CMake
* **`doc/`**: Developer documentation (Markdown format)
* **`doxygen/`**: Doxygen build files and documentation
* **`m4/`**: m4 build scripts for Autotools
* **`release_docs/`**: Install instructions and release notes
* **`utils/`**: Small utility programs

---

## Development Conventions

### Code Organization: Public, Private, and Package

HDF5 code is organized into *packages* that encapsulate related functionality (e.g., `H5D` for datasets). Functions have three visibility levels:

* **Public:** User-facing API functions
  * **Format:** `H5Xfoo()` (e.g., `H5Dcreate`)
  * **Headers:** `H5Xpublic.h`

* **Private:** Internal library API, usable across packages
  * **Format:** `H5X_foo()` (one underscore, e.g., `H5D_create`)

* **Package:** Used only within the defining package
  * **Format:** `H5X__foo()` (two underscores, e.g., `H5D__create`)

### Function Structure

HDF5 functions follow a consistent structure for entry/exit and error handling:

```c
/*
 * Function description
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
        /* error cleanup */
    /* regular cleanup */

    FUNC_LEAVE_NOAPI(ret_value); /* 6. Function leave macro */
}
```

**Public** functions use `FUNC_ENTER_API`, include `H5TRACE` macros for API tracing, and perform more rigorous parameter checking.

### Error Handling

* Almost all functions return `herr_t` or `hid_t` error codes
* `FUNC_ENTER_*` macros set up error handling stack
* `HGOTO_ERROR` pushes errors onto stack and jumps to cleanup
* `FUNC_LEAVE_*` macros return the result
* Always check return values of functions that can fail

### Platform Independence

HDF5 uses a compatibility layer for platform differences:

* Standard C and POSIX calls are prefixed with `HD` (e.g., `HDmalloc`, `HDopen`)
* `H5private.h` and `H5win32defs.h` map these to platform-specific functions
* This layer is being modernized as C99 and POSIX become universal

### Memory Management

Use HDF5's internal memory management instead of direct `malloc`/`free`:

* **`H5MM`:** General-purpose memory management (recommended for most uses)
* **`H5FL`:** Memory pools for fixed-size, frequently allocated objects (use only when performance testing shows clear benefits)

---

## Contributing Changes

### Workflow

1. **Open a GitHub issue** ([HDF5 Issues](https://github.com/HDFGroup/hdf5/issues))
   - **Required** unless the change is minor (e.g., typo fix)
   - Describe the problem or feature request clearly

2. **Fork the repository** and create your branch
   - Target the `develop` branch for new features and bug fixes
   - Use descriptive branch names

3. **Make your changes**
   - Follow HDF5 coding conventions
   - Add tests for new functionality or bug fixes
   - Update documentation as needed

4. **Build and test thoroughly**
   - Follow build instructions in `release_docs/INSTALL*` files
   - Ensure all tests pass

5. **Submit a pull request**
   - Address any formatting or testing issues reported by CI
   - Work with HDF Group developers to meet acceptance criteria

### Acceptance Criteria

For a pull request to be accepted, it must satisfy:

* **Clear purpose:** What does it address? How does it benefit the HDF5 community?
* **Proper documentation:** Code must be documented for maintainability
* **Testing:** Must pass HDF5 regression testing and include appropriate tests
* **Compatibility:** Must not compromise HDF5's core principles:
  - 100% backward compatibility (any HDF5 file must remain readable)
  - Machine independence (data readable across all platforms)
  - Binary compatibility for maintenance releases (no changes to public APIs/structures)
* **Documentation:** New features require proper documentation

### Branching Strategy

* **Small features:** Develop in forks of the main repository
* **Large collaborative work:** Use feature branches named `feature/<feature>` in the main repository
* Add `BRANCH.md` file explaining branch purpose and contact info for feature branches

---

## Testing

### Test Structure

HDF5 uses custom testing macros rather than standard frameworks. There are two systems:

#### Modern Testing (`h5test.h`) - Preferred
```c
#include "h5test.h"

static int
test_feature(void)
{
    TESTING("some feature");
    
    /* test code */
    if (error_condition)
        TEST_ERROR;
    
    PASSED();
    return SUCCEED;

error:
    return FAIL;
}
```

#### Legacy Testing (`testhdf5.h`) - Avoid for New Code
Used only by the large `testhdf5` program. Uses global variables and should be avoided.

### Adding New Tests

**All new functionality and bug fixes must include tests.**

1. Add tests to existing test files when appropriate
2. Create new test programs using `h5test.h` macros
3. Avoid adding to the `testhdf5` program
4. Update `CMakeLists.txt` and `Makefile.am` in the `test/` directory
5. Ensure tests run under both CMake and Autotools

---

## Documentation

### Release Notes

Write release notes for changes that affect users:

#### When to Write Release Notes
- **Required:** User-visible changes in functionality or behavior
- **Required:** Known problems and user-reported issue fixes
- **Not required:** Internal code changes, comments, or build process changes

#### Release Note Format
```
- Title/Problem

  Problem description paragraph explaining the issue and conditions
  where it occurs.

  Solution paragraph describing what was done to resolve the issue
  and any functional impact or workarounds.
```

#### Entry Elements
- **Title:** Categories to help readers identify relevance
- **Problem:** Clear description of the issue and conditions
- **Solution:** What was done, functional impact, and any workarounds

### API Documentation

* **Public functions:** Must have Doxygen markup in `H5Xpublic.h` headers
* **New features:** Document in user guide content in `H5Xmodule.h` files
* **Developer docs:** Internal documentation in `doc/` directory (Markdown)

---

## Command-Line Tools

Tools in the `tools/` directory:
- Written in C using only the **public** HDF5 API
- Organized with central tools library (`tools/lib`) and individual tool directories
- Use simplified error-handling compared to main library
- Examples: `h5dump`, `h5diff`, `h5repack`

---

## Getting Help

### Resources
* **HDF Forum:** Best place for questions about HDF5 usage and development (on HDF Group website)
* **GitHub Issues:** For bug reports and feature requests
* **Documentation:** Check existing docs in `doc/` directory and online resources

### Community
The HDF5 community is here to help. Don't hesitate to reach out with questions or for guidance on contributions.

---

## Checklist for Contributors

Before submitting your pull request, verify:

### Code
- [ ] Corresponding GitHub issue exists (unless minor change)
- [ ] Follows HDF5 conventions (naming, portability, structure)
- [ ] Applicable to other branches? (document in GitHub issue)
- [ ] Sufficiently documented for maintenance
- [ ] API changes follow compatibility guidelines

### Documentation
- [ ] Change described in `release_docs/RELEASE.txt`
- [ ] New functions documented with Doxygen in public headers
- [ ] New features documented for HDF5 community

### Testing
- [ ] Pull request includes tests
- [ ] Consider performance impact

---

Thank you for contributing to HDF5! Your efforts help maintain and improve one of the most important scientific data formats in use today.

