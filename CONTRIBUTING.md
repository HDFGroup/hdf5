# How to contribute to HDF5 development

Welcome to the HDF5 development community! This comprehensive guide covers everything you need to know
about contributing to HDF5, from getting started to submitting your changes.

> [!IMPORTANT]
> No contribution can be accepted unless the contributor agrees to the HDF Group's software license terms,
  which can be found in the LICENSE file located in the top source directory of every branch.

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
- [Command-Line Tools](#command-line-tools)
- [Checklist for Contributors](#checklist-for-contributors)
- [Getting Help](#getting-help)


---

## Getting Started

The HDF Group welcomes contributions of all kinds, from fixing typos to adding significant features. We are
dedicated to making the contribution process enjoyable and straightforward.

> [!NOTE]
> This guide offers a brief introduction to the HDF5 library and its development procedures. In contrast,
  [An Overview of the HDF5 Library Architecture][u1] aims to provide a comprehensive understanding of the inner workings
  of the HDF5 library by exploring its fundamental principles. It covers the systematic, structural, and organized aspects
  that enable the library to function clearly and effectively. By reviewing this document, readers can gain insights into
  the library's architecture and learn how to use it efficiently. Additionally, it will provide an overview of the various
  approaches used to simplify the understanding of the HDF5 library's operations.

---

## Prerequisites

Before you begin, ensure your development machine has:

### Required Tools
* **A C11-compatible C compiler** (MSVC on Windows is supported).
* **A build system:** **CMake** is required.
* **Perl:** Needed to run build and test scripts, even on Windows.
* **Git:** For version control.
  - If you are new to Git and GitHub, we encourage you to check out
    the [GitHub tutorial](https://guides.github.com/activities/hello-world/), which takes about 10 minutes to complete.

### Recommended Tools
* **clang-format:** For code formatting. The CI system will automatically format pull requests if needed.
* **codespell:** For identifying spelling issues before submission.
* **Doxygen:** For compiling the documentation.

### Optional Components
Depending on which features you want to build or enable:
* A _C++11_-compatible compiler for the C++ wrappers.
* A _Fortran 2003_-compatible compiler for the Fortran wrappers.
* A _Java 8_-compatible compiler for the Java wrappers.
* **Maven** for Java artifact deployment and validation (when `HDF5_ENABLE_MAVEN_DEPLOY=ON`).
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

### Basic CMake Build

CMake is the required build system for all platforms:

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

### Developer Build Tips

* **Memory Checking:** Use `HDF5_ENABLE_USING_MEMCHECKER:BOOL=ON` when using tools like Valgrind. This disables
                       internal memory pools that can hide memory issues.
* **Developer Warnings:** Enable extra warnings with `HDF5_ENABLE_DEV_WARNINGS:BOOL=ON` (generates significant
                          output but can be useful).
* **Warnings as Errors:** The CI system builds with `-Werror`, so fix all compiler warnings before submitting pull requests.

### Maven Integration Development

For developers working on Java bindings and Maven integration:

* **Enable Maven Support:** Use `HDF5_ENABLE_MAVEN_DEPLOY:BOOL=ON` to enable Maven artifact generation.
* **Snapshot Builds:** Use `HDF5_MAVEN_SNAPSHOT:BOOL=ON` for development builds with `-SNAPSHOT` versions.
* **Maven Presets:** Use Maven-enabled CMake presets for consistent builds:
  ```bash
  # For full builds (includes all components)
  cmake --workflow --preset ci-StdShar-GNUC-Maven-Snapshot --fresh

  # For Java artifact generation only (recommended for Maven development)
  cmake --workflow --preset ci-MinShar-GNUC-Maven-Snapshot --fresh
  ```
* **Artifact Validation:** Test Maven artifacts using `.github/scripts/validate-maven-artifacts.sh` script.
* **Repository Testing:** Use the `maven-staging.yml` workflow for pull request validation.

---

## Source Code Overview

Here's where to find things in the source tree:

* **`src/`**: Main C library source code
* **`test/`**: C library test code
* **`testpar/`**: Parallel C library test code
* **`tools/`**: Command-line tools (h5dump, h5repack, etc.)
* **`HDF5Examples/`**: Library examples including Java examples with Maven integration
* **`hl/`**: High-level library source, tests, and examples
* **`c++/`**: C++ language wrapper
* **`fortran/`**: Fortran language wrapper
* **`java/`**: JNI/Java language wrapper
* **`bin/`**: Build scripts and miscellaneous tools
* **`config/`**: Configuration files for CMake
* **`doxygen/`**: Doxygen build files and documentation
* **`release_docs/`**: Install instructions and release notes
* **`utils/`**: Small utility programs

---

## Development Conventions

### Code Organization: Public, Private, and Package

HDF5 code is organized into *packages* that encapsulate related functionality (e.g., `H5D` for datasets).
Functions have three visibility levels:

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
   - **Required** unless the change is minor (e.g., typo fix).
   - Describe the problem or feature request clearly.

2. **Fork the repository** and create your branch
   - Target the `develop` branch for new features and bug fixes.
   - Use descriptive branch names.

3. **Make your changes**
   - Follow HDF5 coding conventions.
   - Add tests for new functionality or bug fixes.
   - Update documentation as needed.

4. **Build and test thoroughly**
   - Follow build instructions in `release_docs/INSTALL*` files.
   - Ensure all tests pass.

5. **Submit a pull request (PR)**
   - Address any formatting or testing issues reported by CI.
   - Make sure to include the issue that the PR addresses in the description.
   - Work with HDF Group developers to meet acceptance criteria.

### Acceptance Criteria

For a pull request to be accepted, it must satisfy:

* **Clear purpose:** What does it address? How does it benefit the HDF5 community?
* **Proper documentation:** Code must be documented for maintainability.
* **Testing:** Must pass HDF5 regression testing and include appropriate tests.
  - We do not expect you to perform comprehensive testing across multiple platforms
    before we accept the pull request.
* **Compatibility:** Must not compromise HDF5's core principles:
  - 100% backward compatibility (any HDF5 file must remain readable).
    - If your patch's purpose is to modify the HDF5 data model or file format, **please** discuss
      this with us first. File format changes and features required by those changes can be
      introduced only in a new major release.
  - Machine independence (data readable across all platforms).
  - Binary compatibility for maintenance releases (no changes to public APIs/structures).
* **Documentation:** New features must be properly documented. This includes using Doxygen
    and providing information in release documents such as `CHANGELOG.md`.

### Branching Strategy

* **Small features:** Develop in forks of the main repository.
* **Large collaborative work:** Use feature branches named `feature/<feature>` in the main repository.
* Add `BRANCH.md` file explaining branch purpose and contact info for feature branches.

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

1. Add tests to existing test files when appropriate.
2. Create new test programs using `h5test.h` macros.
3. Avoid adding to the `testhdf5` program.
4. Update `CMakeLists.txt` in the `test/` directory.
5. Ensure tests run and pass under CMake.

### Maven Deployment Testing

For contributions involving Maven deployment or Java bindings:

1. **Test Maven Artifacts:** Use the validation script to verify artifact generation:
   ```bash
   # Build with Maven support
   cmake --workflow --preset ci-MinShar-GNUC-Maven-Snapshot --fresh

   # Validate generated artifacts
   .github/scripts/validate-maven-artifacts.sh build/ci-MinShar-GNUC-Maven-Snapshot
   ```

2. **PR Validation:** The `maven-staging.yml` workflow automatically tests Maven artifacts for pull requests when Java-related files are modified.

3. **Multi-Platform Testing:** Verify artifacts generate correctly on all platforms by testing with different Maven presets:
   - Linux: `ci-MinShar-GNUC-Maven-Snapshot`
   - Windows: `ci-MinShar-MSVC-Maven-Snapshot`
   - macOS: `ci-MinShar-Clang-Maven-Snapshot`

4. **Dry Run Testing:** Before deploying to repositories, test deployment permissions using the dry run mode in release workflows.

5. **Java Examples Testing:** The Java examples Maven integration includes comprehensive testing:
   ```bash
   # Test Java examples with Maven artifacts (all platforms)
   gh workflow run maven-staging.yml -f platforms=all-platforms

   # Run dedicated Java examples testing
   gh workflow run java-examples-maven-test.yml -f category=all
   ```
   - **Cross-Platform Validation:** Ensures examples work with platform-specific Maven artifacts
   - **Native Library Error Handling:** Validates JAR structure through expected native library errors
   - **Multi-Platform Coverage:** Tests on Linux, Windows, macOS x86_64, and macOS aarch64

---

## Documentation

### Release Notes

Write release notes for changes that affect users:

#### When to Write Release Notes
- **Required:** User-visible changes in functionality or behavior.
- **Required:** Known problems and user-reported issue fixes.
- **Not required:** Internal code changes, comments, or build process changes.

#### Release Note Format
```
- Title/Problem

  Problem description paragraph explaining the issue and conditions
  where it occurs.

  Solution paragraph describing what was done to resolve the issue
  and any functional impact or workarounds.
```

#### Entry Elements
- **Title:** Categories to help readers identify relevance.
- **Problem:** Clear description of the issue and conditions.
- **Solution:** What was done, functional impact, and any workarounds.

### API Documentation

* **Public functions:** Must have Doxygen markup in `H5Xpublic.h` headers.
* **New features:** Document in user guide content in `H5Xmodule.h` files.
* **Developer docs:** By means of well documented source.

---

## Command-Line Tools

Tools in the `tools/` directory:
- Written in C using only the **public** HDF5 API.
- Organized with central tools library (`tools/lib`) and individual tool directories.
- Use simplified error-handling compared to main library.
- Examples: `h5dump`, `h5diff`, `h5repack`.

---

## Checklist for Contributors

Before submitting your pull request, verify:

### Code
- [ ] Corresponding GitHub issue exists (unless minor change).
- [ ] Follows HDF5 conventions (naming, portability, structure).
- [ ] Applicable to other branches? (document in GitHub issue).
- [ ] Sufficiently documented for maintenance.
- [ ] API changes follow compatibility guidelines.

### Documentation
- [ ] Change described in `release_docs/CHANGELOG.md`.
- [ ] New functions documented with Doxygen in public headers.
- [ ] New features documented for HDF5 community.

### Testing
- [ ] Pull request includes tests.
- [ ] Consider performance impact.

---

## Getting Help

### Resources
* **HDF Forum:** Best place for questions about HDF5 usage and development (on HDF Group website).
* **GitHub Issues:** For bug reports and feature requests.
* **Documentation:** Check existing docs on the HDF Group website.

### Community
The HDF5 community is here to help. Don't hesitate to reach out with questions or for guidance on contributions.

---

Thank you for contributing to HDF5! Your efforts help maintain and improve one of the most widely used data formats today.

[u1]: https://github.com/HDFGroup/arch-doc/blob/main/An_Overview_of_the_HDF5_Library_Architecture.v2.pdf
