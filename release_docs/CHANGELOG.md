v2.2.0 --- January X , 2026

# 🔺 HDF5 Changelog
All notable changes to this project will be documented in this file. This document describes the differences between this release and the previous
HDF5 release, platforms tested, and known problems in this release.

For releases prior to version 2.0.0, please see the release.txt file and for more details check the HISTORY*.txt files in the HDF5 source.

# 🔗 Quick Links
* [HDF5 documentation](https://support.hdfgroup.org/documentation/hdf5/latest/)
* [Official HDF5 releases](https://support.hdfgroup.org/downloads/index.html)
* [Changes from Release to Release and New Features in the HDF5-2.x.y](https://support.hdfgroup.org/releases/hdf5/documentation/release_specific_info.md)
* [Getting help, questions, or comments](https://github.com/HDFGroup/hdf5#help-and-support)

## 📖 Contents
* [Executive Summary](CHANGELOG.md#execsummary)
* [Breaking Changes](CHANGELOG.md#%EF%B8%8F-breaking-changes)
* [Deprecations](CHANGELOG.md#-deprecations)
* [New Features & Improvements](CHANGELOG.md#-new-features--improvements)
* [Bug Fixes](CHANGELOG.md#-bug-fixes)
* [Support for new platforms and languages](CHANGELOG.md#-support-for-new-platforms-and-languages)
* [Platforms Tested](CHANGELOG.md#%EF%B8%8F-platforms-tested)
* [Known Problems](CHANGELOG.md#-known-problems)

# 🔆 Executive Summary: HDF5 Version 2.2.0

> [!IMPORTANT]
>
> - The format of the GitHub tag for HDF5 releases has been changed to Major.Minor.Patch, consistent with the versioning policy change to follow the Semantic Versioning Specification described in this [Wiki page](https://github.com/HDFGroup/hdf5/wiki/HDF5-Version-Numbers-and-Branch-Strategy).  The previous tag format hdf5_Major_Minor_Patch that was created in addition for the 2.0.0 and 2.1.0 releases will not be continued.
> - An RPM package is not provided for this release of HDF5 as an issue with the package was found during testing. The HDF Group is investigating alternative packaging methods for future releases.


## Performance Enhancements:


## Significant Advancements:


## Enhanced Features:

- Made several improvements to the CMake logic for handling filter libraries

## Java Enhancements:

- Java dependency JAR paths are now configurable CMake cache variables, allowing system-provided JARs to be used in place of the bundled copies.


## Acknowledgements:

We would like to thank the many HDF5 community members who contributed to this release of HDF5.

# ⚠️ Breaking Changes

# 🪦 Deprecations

- The CMake variable `ZLIB_GIT_BRANCH` has been deprecated in favor of `ZLIB_GIT_TAG`
- The CMake variable `ZLIBNG_GIT_BRANCH` has been deprecated in favor of `ZLIBNG_GIT_TAG`
- The CMake variable `LIBAEC_GIT_BRANCH` has been deprecated in favor of `LIBAEC_GIT_TAG`
- The CMake variable `PLUGIN_GIT_BRANCH` has been deprecated in favor of `HDF5_FILTER_PLUGINS_GIT_TAG`
- The CMake variable `PLUGIN_GIT_URL` has been deprecated in favor of `HDF5_FILTER_PLUGINS_GIT_URL`
- The CMake variable `PLUGIN_TGZ_NAME` has been deprecated in favor of `HDF5_FILTER_PLUGINS_TGZ_NAME`
- The CMake variable `PLUGIN_TGZ_ORIGPATH` has been deprecated in favor of `HDF5_FILTER_PLUGINS_TGZ_ORIGPATH`
- The CMake variable `PLUGIN_PACKAGE_NAME` has been deprecated in favor of `HDF5_FILTER_PLUGINS_PACKAGE_NAME`

# 🚀 New Features & Improvements

## Configuration

### Consolidated documentation under docs/ directory

   User-facing guides (installation, build instructions, platform-specific docs) and
   Doxygen API documentation have been consolidated under a new top-level `docs/`
   directory. All internal references (CMakeLists.txt, README.md, workflow files,
   Doxygen sources, scripts, etc.) have been updated accordingly.

### Updated external building of zlib, zlib-ng and libaec to not use a patching process

   When building these libraries from external sources while building HDF5, the library previously used a patching process to adapt the libraries to its own build process. The sources for these libraries are no longer patched and build directly from the sources of the latest upstream releases (currently, zlib 1.3.2, zlib-ng 2.3.3 and libaec 1.1.6). This also fixed an issue with the build of zlib-ng failing due to updates that were made since the last version that HDF5 was patching the sources for.

   Fixes GitHub issue #6204

### Fixed an issue where CMake-built installations of zlib libraries couldn't be located on a system

   An incorrect package name was being supplied to CMake's find_package() function when attempting to locate zlib libraries on the system in Config mode. The package name has been corrected and CMake-built zlib libraries can now be located.

### Fixed an issue where static zlib libraries couldn't be found on the system

   The value of the HDF5 CMake variable `HDF5_USE_ZLIB_STATIC` was previously used incorrectly when locating zlib libraries on the system with CMake's find_package() function, causing it to have no effect. This has been fixed and static zlib libraries can now be located.

### Added a CMake module to locate zlib-ng for zlib support

   A new `FindZLIBNG.cmake` CMake module has been added. This module is intended to locate zlib-ng on the system for zlib support in HDF5 when zlib-ng was built with Autotools instead of CMake. When zlib-ng support is enabled in HDF5 with the `HDF5_ENABLE_ZLIB_SUPPORT` and `HDF5_USE_ZLIB_NG` options, this module will first check for an existing CMake-built zlib-ng and use that if it's available. Otherwise, the module will heuristically search for zlib-ng on the system. If necessary, the module can be hinted toward a particular zlib-ng installation by setting the CMake variable `ZLIBNG_ROOT` to point to a directory.

### Added a CMake module to locate libaec for SZIP support

   A new `Findlibaec.cmake` CMake module has been added. This module is intended to locate libaec on the system for SZIP support in HDF5 when libaec was built with Autotools instead of CMake. When SZIP support is enabled in HDF5 with the `HDF5_ENABLE_SZIP_SUPPORT` option, this module will first check for an existing CMake-built libaec and use that if it's available. Otherwise, the module will heuristically search for libaec on the system. If necessary, the module can be hinted toward a particular libaec installation by setting the CMake variable `libaec_ROOT` to point to a directory. If it is known that a CMake-built libaec installation exists on the system in a non-standard location, the CMake variable `libaec_DIR` can instead be set to a directory containing a `libaec-config.cmake` file to cause the module to prefer that libaec installation.

## Library

### Added optional digital signature verification for dynamically loaded plugins

   When built with `-DHDF5_REQUIRE_SIGNED_PLUGINS=ON` and OpenSSL, HDF5 will cryptographically verify each plugin before loading it. Plugins are signed with the new `h5sign` tool, which appends an RSA signature and a compact footer to the plugin binary. Verification uses a keystore directory of trusted public keys, configurable at compile time (`-DHDF5_PLUGIN_KEYSTORE_DIR=<path>`) or at runtime via the `HDF5_PLUGIN_KEYSTORE` environment variable. Individual signatures can be revoked without removing the entire public key by listing their SHA-256 hashes in a `revoked_signatures.txt` file in the keystore directory. Supported algorithms include SHA-256, SHA-384, and SHA-512 with both PKCS#1 v1.5 and PSS padding. See `docs/PLUGIN_SIGNATURE_README.md` for details.

### Improve performance of H5Ovisit() with deeply nested group structures

   `H5Ovisit()` would previously internally traverse each object's path name from the iteration root group in order to retrieve information about that object, causing severe performance degradation with a deeply nested group structure. Modified the algorithm to instead retrieve information directly from the object. To get this benefit, users should use `H5Ovisit3()`, or use `H5Ovisit2()` with neither `H5O_INFO_HDR` nor `H5O_INFO_META_SIZE` selected in the `fields` parameter. Performance of `H5Ocopy()`, `H5Iget_name()`, and external links with a callback set should also improve in similar situations.

### Versioned API functions now default to earliest version for older API settings

   When a global API compatibility version is set (e.g., `H5_USE_16_API`), functions introduced after that version previously defaulted to their latest version, which could break applications. For example, an application using `H5_USE_16_API` that called `H5Sencode()` (introduced in 1.8, versioned in 1.12) would get `H5Sencode2()` instead of `H5Sencode1()`, potentially causing compilation or runtime failures. Versioned functions now default to their earliest (version 1) variant when the configured API level predates the function's introduction, providing maximum compatibility. See issue [#6278](https://github.com/HDFGroup/hdf5/issues/6278).

## Parallel Library

## Fortran Library

## C++ Library

## Java Library

### Java dependency JAR paths are now user-configurable

   The CMake variables `HDF5_JAVA_LOGGING_JAR`, `HDF5_JAVA_LOGGING_NOP_JAR`, `HDF5_JAVA_LOGGING_SIMPLE_JAR`, `HDF5_JAVA_JUNIT_JAR`, and `HDF5_JAVA_HAMCREST_JAR` are now CMake cache variables with the bundled JARs as defaults. Users can override these at configure time to use system-provided JARs. See `INSTALL_CMake_options.md` for details.

## Tools

### Default low and high library version bounds in `h5repack` now use the HDF5 library's default

The `h5repack` tool now obtains its default low and high library version bounds from the HDF5 library's default (`H5P_FILE_ACCESS_DEFAULT`). To revert to the previous behavior, apply the `--low=0` command option.

### Added `h5sign` tool for signing plugins with RSA digital signatures

   The `h5sign` command-line tool signs HDF5 plugin shared libraries by appending an RSA signature and a 14-byte footer. It supports SHA-256, SHA-384, SHA-512, and their PSS variants, and accepts passphrase-protected private keys. Use `-f` / `--force` to strip an existing signature before re-signing. The tool is built automatically when `HDF5_REQUIRE_SIGNED_PLUGINS` is enabled.

## High-Level APIs

## C Packet Table API

## Internal header file

## Documentation


# 🪲 Bug Fixes

## Library

### HTTP 403 errors in the ROS3 VFD for object keys with special characters

   The ROS3 VFD did not URI-encode the S3 object key when building the HTTP request path, so keys containing characters that AWS Signature Version 4 requires to be percent-encoded — such as the '=' in Hive-style `key=value` partition prefixes, '+', or spaces — produced a signed request whose signature did not match S3's server-side recomputation. S3 rejects such requests with `SignatureDoesNotMatch`, which surfaces as an HTTP 403 error (indistinguishable from a permissions error on a HEAD request), even though tools like the AWS CLI could access the same object. The object key is now percent-encoded exactly once when the request path is built, matching the behavior of other S3 clients. Note that URLs must now be passed to the ROS3 VFD with their object keys unencoded; a key that was pre-encoded as a workaround for this issue will now be double-encoded and fail to resolve.

### Fixed file descriptor leaks in stdio VFD error paths

   Fixed multiple resource leaks in the H5FDstdio driver where file descriptors were not properly closed on error paths. The error handling code was incorrectly attempting to close a local variable instead of the file pointer stored in the file structure, leading to file descriptor leaks. This issue affected 5 error paths in `H5FD_stdio_open()` and could cause file descriptor exhaustion in long-running applications.

### Added defensive NULL pointer checks in native VOL connector

   Added assertion checks for NULL pointer parameters in `H5VL_native_get_file_struct()` to catch programming errors earlier and improve code robustness.

### Added checks for data filter behavior

   The library now verifies that the returned data size from a data filter's filter callback function can fit inside the returned data buffer size. The library also checks that, when data is filtered then unfiltered (filtered in reverse), the returned data size is exactly the same as the original data size.

### Fixed bugs with chunk buffer handling

   Fixed a bug in the deflate filter that caused it to report the wrong buffer size. Fixed a bug in the chunk copy code that could cause a background buffer overflow. Fixed a bug in the chunk copy code that could cause a double free if the filter realloced the data buffer.

### Fixed checking of data alignment requirements in direct I/O VFD

   The direct I/O VFD attempts to determine data alignment requirements for a file on file open to try and avoid extra work when data alignment isn't required. Depending on the file access flags used when opening a file, the VFD could incorrectly determine these requirements for either writes or reads, eventually leading to a possible EINVAL return value on write or read. This has been fixed by separately determining the requirements for writes and reads and being more conservative about trying to avoid data alignment requirements.

### Fixed integer overflow in array datatype element count computation

   Fixed a bug in H5O__dtype_decode_helper() where the loop computing the total number of elements in an array datatype had no per-step overflow check. On 64-bit systems, large dimension sizes could cause the element count to wrap around, bypassing the post-loop overflow check and producing silently incorrect results in downstream type conversion and size calculations.

### Fixed an issue with chunked datasets using the wrong index type with parallel HDF5

   Fixed a bug in parallel HDF5 that would cause chunked datasets with fixed dimensions and without filters applied to use the "none" index type instead of the "fixed array" index type.

### Fixed an issue with decoding metadata cache image superblock extension messages

   Fixed a bug where loading of a metadata cache image superblock extension message would fail when the image had an undefined address and size of 0.

### Fixed an issue with an incorrect file format validation check when decoding metadata cache entries

   Fixed a bug where a flag in H5Cimage.c wasn't getting set correctly for release builds of HDF5, leading to incorrect error checking when reconstructing metadata cache entries.

## Java Library

## Configuration

## Tools

### Fixed h5repack silently dropping a declared cd_nelmts for user-defined filters

   `h5repack -f UD=<filtn>,<flag>,<cd_nelmts>` with no values following `cd_nelmts` silently
   treated the declared count as 0 instead of validating it, because the trailing token (with
   no comma after it) was never committed to `cd_nelmts` inside the parser. `parse_filter()` now
   commits the trailing token to whichever UD field is still pending, so a declared, unfulfilled
   `cd_nelmts` is correctly rejected with "incorrect number of compression parameters" instead of
   being silently coerced to 0.

## Performance

## Fortran API

## High-Level Library

### Fixed critical buffer overflow vulnerability in H5TBget_field_info() (CWE-120)

   `H5TBget_field_info()` copied field names into caller-provided buffers using unbounded `strcpy()`,
   allowing a malicious HDF5 file with overly long field names to overflow those buffers. The copy
   now uses bounds-checked `memcpy()`: names shorter than `HLTB_MAX_FIELD_LEN` (255) are copied
   exactly (preserving backward compatibility); names at or above that limit are safely truncated to
   254 characters plus a NUL terminator.

### Made HLTB_MAX_FIELD_LEN public

   `HLTB_MAX_FIELD_LEN` (255) has been moved from the private header `H5TBprivate.h` to the public
   header `H5TBpublic.h`. Applications can now use this constant to correctly size their
   `field_names[]` buffers when calling `H5TBget_field_info()`.

### Fixed memory leaks and improved safety in H5LT functions

   - Fixed memory leak in `H5LTtext_to_dtype()` by adding NULL check after `strdup()` call
   - Added defensive NULL checks and pointer nullification after `free()` calls to prevent use-after-free bugs
   - Improved documentation for `realloc_and_append()` internal function with detailed parameter contracts and preconditions

### Eliminated code duplication in H5LT datatype conversion

   Refactored `H5LT_dtype_to_text()` by extracting common super-type handling logic into a new helper function `H5LT_append_dtype_super_text()`. This eliminates approximately 80 lines of duplicated code that was previously repeated across 4 datatype cases (ENUM, VLEN, ARRAY, COMPLEX), improving maintainability and reducing the risk of inconsistent behavior.

### Fixed H5TBread_fields_name/H5TBwrite_fields_name matching the wrong field when one field name is a prefix of another

   H5TB_find_field() used strncmp() limited to strlen(field) when comparing the last entry of the supplied comma-separated field list against a table member name. This matched any user-supplied name whose leading characters equaled an existing field name (for example, requesting "PressureExtra" on a table containing "Pressure" would silently operate on the "Pressure" field). The comparison has been changed to strcmp() so full names must match exactly. In addition, H5TBwrite_fields_name() now returns an error when none of the requested field names are found (previously it silently performed a no-op write), matching the existing behavior of H5TBread_fields_name().

   Fixes GitHub issue #5633

### Fixed prefix-based false matches when checking "CLASS" attribute strings in the High-Level API

   `H5DSis_scale()`, `H5DS_is_reserved()`, `H5IMis_image()`, and `H5IMis_palette()` all compared a
   dataset's "CLASS" attribute against an expected class name using
   `strncmp(buf, CLASS, MIN(strlen(CLASS), strlen(buf)))`. Because the comparison was limited to the
   shorter of the two strings, any non-empty value whose leading characters matched the expected class
   name was accepted — for example, a CLASS of `"IMAGE_EXTRA"` was treated as an IMAGE dataset, and
   `"DIMENSION_S"` (null-padded to 16 bytes) was treated as a DIMENSION_SCALE. (`H5DSis_scale()` already
   required the attribute datatype to be exactly 16 bytes, which incidentally prevented false matches
   against shorter class names such as `"IMAGE"` or `"PALETTE"`; the other three functions had no such
   guard and were directly exposed.) These
   comparisons now use `strcmp()` so only an exact class name is accepted.

   Additional fixes applied to all four routines:

   - **VLEN-string CLASS attributes are now handled correctly.** Previously, reading a VLEN-typed
     attribute into a fixed `char *` buffer would overwrite it with a heap-allocated `char *` pointer
     rather than the string content, which is undefined behaviour and could corrupt memory or produce
     garbage comparison results.
     All four routines now read VLEN CLASS attributes properly (via `H5Treclaim`) and compare the
     string content: `H5DSis_scale()`, `H5IMis_image()`, and `H5IMis_palette()` return 1 when the
     value matches exactly, and `H5DS_is_reserved()` correctly identifies reserved class names stored
     as VLEN strings.
   - **NUL-termination hardening.** The read buffer is now allocated one byte larger than the stored
     attribute size, and a NUL terminator is explicitly written after the attribute data. This protects
     `strcmp` from over-reading files where the CLASS attribute was written without strictly honouring
     `H5T_STR_NULLTERM`.
   - **Resource leak fix in `H5IMis_image()` and `H5IMis_palette()`.** The `out:` error-handling block
     previously closed only the dataset ID, leaving the attribute ID (`aid`) and attribute datatype ID
     (`atid`) open on every error path. Both IDs are now properly closed on error.

   Related to GitHub issue #5633

## Fortran High-Level APIs

## Documentation

## F90 APIs

## C++ APIs

## Testing

# ✨ Support for new platforms and languages

# ☑️ Platforms Tested

A table of platforms tested can be seen on the [wiki](https://github.com/HDFGroup/hdf5/wiki/Platforms-Tested).
Current test results are available [here](https://my.cdash.org/index.php?project=HDF5).

# ⛔ Known Problems

- When performing implicit datatype conversion on specific non-IEEE floating-point format data, HDF5 may improperly convert some data values:

   When performing I/O operations using a non-IEEE floating-point format datatype, HDF5 may improperly convert some data values due to incomplete handling of non-IEEE types. Such types include the following pre-defined datatypes:

    H5T_FLOAT_F8E4M3
    H5T_FLOAT_F8E5M2
    H5T_FLOAT_F6E2M3
    H5T_FLOAT_F6E3M2
    H5T_FLOAT_F4E2M1

   If possible, an application should perform I/O with these datatypes using an in-memory type that matches the specific floating-point format and perform explicit data conversion outside of HDF5, if necessary. Otherwise, read/written values should be verified to be correct.

- When the library detects and builds in support for the _Float16 datatype, an issue has been observed on at least one MacOS 14 system where the library fails to initialize due to not being able to detect the byte order of the _Float16 type [#4310](https://github.com/HDFGroup/hdf5/issues/4310):

     #5: H5Tinit_float.c line 308 in H5T__fix_order(): failed to detect byte order
     major: Datatype
     minor: Unable to initialize object

   If this issue is encountered, support for the _Float16 type can be disabled with a configuration option:

     `CMake: HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16=OFF`

- When HDF5 is compiled with NVHPC versions 23.5 - 23.9 (additional versions may also be applicable) and with -O2 (or higher) and -DNDEBUG, test failures occur in the following tests:

   - H5PLUGIN-filter_plugin
   - H5TEST-flush2
   - H5TEST-testhdf5-base
   - MPI_TEST_t_filters_parallel

  Sporadic failures (even with lower -O levels):

   - Java JUnit-TestH5Pfapl
   - Java JUnit-TestH5D

  Also, NVHPC will fail to compile the test/tselect.c test file with a compiler error of `use of undefined value` when the optimization level is -O2 or higher.

   This is confirmed to be a [bug in the nvc compiler](https://forums.developer.nvidia.com/t/hdf5-no-longer-compiles-with-nv-23-9/269045) that has been fixed as of 23.11. If you are using an affected version of the NVidia compiler, the work-around is to set the optimization level to -O1.

- CMake files do not behave correctly with paths containing spaces

   Do not use spaces in paths because the required escaping for handling spaces results in very complex and fragile build files.

- At present, metadata cache images may not be generated by parallel applications. Parallel applications can read files with metadata cache images, but since this is a collective operation, a deadlock is possible if one or more processes do not participate.

- The subsetting option in `ph5diff` currently will fail and should be avoided

   The subsetting option works correctly in serial `h5diff`.

- Flang Fortran compilation will fail (last check version 17) due to not yet implemented: (1) derived type argument passed by value (H5VLff.F90), and (2) support for REAL with KIND = 2 in intrinsic SPACING used in testing.

- Fortran tests HDF5_1_8.F90 and HDF5_F03.F90 will fail with Cray compilers greater than version 16.0 due to a compiler bug. The latest version verified as failing was version 17.0.

- Several tests currently fail on certain platforms:
   MPI_TEST-t_bigio fails with spectrum-mpi on ppc64le platforms.

   MPI_TEST-t_subfiling_vfd and MPI_TEST_EXAMPLES-ph5_subfiling fail with
   cray-mpich on theta and with XL compilers on ppc64le platforms.

- File space may not be released when overwriting or deleting certain nested variable length or reference types.

Known problems in previous releases can be found in the HISTORY*.txt files in the HDF5 source. Please report any new problems found to <a href="mailto:help@hdfgroup.org">help@hdfgroup.org</a>.
