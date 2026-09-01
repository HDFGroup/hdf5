v2.3.0 --- July X , 2026

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

# 🔆 Executive Summary: HDF5 Version 2.3.0


## Performance Enhancements:


## Significant Advancements:


## Enhanced Features:


## Java Enhancements:


## Acknowledgements:

We would like to thank the many HDF5 community members who contributed to this release of HDF5.

# ⚠️ Breaking Changes


# 🪦 Deprecations


# 🚀 New Features & Improvements

## Configuration


## Library

### Added string-based filter configuration API (RFC-HDFG-2026-001)

   A new API allows filters to be configured using human-readable `key=value`
   parameter strings (TOML-subset syntax) in addition to the existing integer
   `cd_values` arrays.

   **New C API functions:**
   - `H5Pappend_filter(plist, filter_id, flags, params)` - appends a filter to
     a dataset creation property list; `params` is an `H5Z_params_t` that
     carries either a `key=value` string or a raw `cd_values` array.
   - `H5Pmodify_filter_by_idx(plist, filter_idx, flags, params)` - replaces
     the configuration of the filter already at pipeline index `filter_idx`,
     preferred over `H5Pmodify_filter()` for a string-configured entry since
     it preserves the stored configuration string when given a new string.
   - `H5Pget_filter_params_by_idx(plist, idx, buf, buf_size, content_len)` -
     retrieves the parameter string for the filter at pipeline index `idx`.
   - `H5Zconfig_get_int`, `H5Zconfig_get_double`, `H5Zconfig_get_bool`,
     `H5Zconfig_get_str` - typed accessors that extract individual parameters
     from a `key=value` string; intended for use inside filter `set_config`
     callbacks.
   - `H5Zget_filter_class_info(filter_id, info)` - returns registry-level
     information about a filter (canonical name, description, whether
     `set_config`/`get_config` are implemented) into a new `H5Z_class_info_t`
     output struct.

   **New filter class fields:**  `H5Z_class3_t` gains a `name` string field
   (canonical identifier used in config strings) and a `description` string
   field (human-readable display name), plus `set_config` / `get_config`
   callbacks that translate between `key=value` strings and internal state.

   **TOML subset parser:**  The [tomlc17](https://github.com/cktan/tomlc17)
   library is now vendored in `src/tomlc17/` and compiled unconditionally into
   libhdf5.  Hex-float literals (`0x1.8p+1`) in parameter strings are
   transparently rewritten to decimal before parsing.

   **On-disk format:** A new pipeline message version, `H5O_PLINE_VERSION_3`,
   stores each filter's verbatim parameter string after the filter name, so
   the exact string can be recovered without loading the filter plugin.
   `H5Pget_filter_params_by_idx` returns that stored string when present and
   otherwise falls back to the filter's `get_config` callback, then to a
   `cd_values` listing.  Version 3 is written only when a filter carries a
   stored string and the file's high library-version bound admits it (see
   `H5F_LIBVER_V300` below); otherwise the message is written at version 2
   with the string omitted, so files without stored strings remain
   byte-identical to previous releases.  `H5Pmodify_filter` clears the stored
   string for the modified filter; `H5Pcopy`, `H5Pencode`/`H5Pdecode`, and
   `H5Ocopy` carry it with the entry.

   Fixes GitHub issue [#6153](https://github.com/HDFGroup/hdf5/issues/6153)

### Changed: `H5Pget_filter2`/`H5Pget_filter_by_id2` no-name fallback

   When a filter has no stored or registered name, `H5Pget_filter2()` and
   `H5Pget_filter_by_id2()` now write the filter's decimal ID (e.g. `"307"`)
   into the output `name[]` buffer, rather than an empty string as in prior
   releases. Existing code that tested `name[0] == '\0'` to detect "no name
   available" will no longer see that sentinel; check the return value and
   `namelen` instead if that distinction is needed.

### Added the H5F_LIBVER_V300 library version bound

   The `H5F_libver_t` enumeration gains `H5F_LIBVER_V300`, and
   `H5F_LIBVER_LATEST` now maps to it.  A file access property list's high
   bound must be at least `H5F_LIBVER_V300` to write the version-3 filter
   pipeline message (used to persist filter parameter strings); all other
   message versions are unchanged from `H5F_LIBVER_V200`.  The constant is
   mirrored in the Fortran (`H5F_LIBVER_V300_F`) and Java
   (`HDF5Constants.H5F_LIBVER_V300`) bindings.

### Added an I/O block cache to the ROS3 VFD

   Added an I/O block cache to the ROS3 VFD to reduce the number of requests to S3 for files that don't use paged allocation. This is a simple LRU cache that performs I/O in fixed-size blocks and serves I/O requests from the in-memory cached buffers. By default, the ROS3 VFD now performs I/O in 16 MiB (see new macro `HDF5_ROS3_VFD_DEFAULT_BLOCK_SIZE`) blocks, caching up to a total of 128 MiB (see new macro `HDF5_ROS3_VFD_DEFAULT_BLOCK_CACHE_SIZE`) of data at a time. The new `H5Pset_fapl_ros3_block_caching()` / `H5Pget_fapl_ros3_block_caching()` API functions can be used to modify or retrieve the caching parameters set on a File Access Property List, respectively. Additionally, caching of the initial bytes of a file has been delayed from file open to the first read of a file instead to reduce the overhead of file opens.

### Added optional digital signature verification for dynamically loaded plugins

   When built with `-DHDF5_REQUIRE_SIGNED_PLUGINS=ON` and OpenSSL, HDF5 will cryptographically verify each plugin before loading it. Plugins are signed with the new `h5sign` tool, which appends an RSA signature and a compact footer to the plugin binary. Verification uses a keystore directory of trusted public keys, configurable at compile time (`-DHDF5_PLUGIN_KEYSTORE_DIR=<path>`) or at runtime via the `HDF5_PLUGIN_KEYSTORE` environment variable. Individual signatures can be revoked without removing the entire public key by listing their SHA-256 hashes in a `revoked_signatures.txt` file in the keystore directory. Supported algorithms include SHA-256, SHA-384, and SHA-512 with both PKCS#1 v1.5 and PSS padding. See `docs/PLUGIN_SIGNATURE_README.md` for details.

### Improve performance of H5Ovisit() with deeply nested group structures

   `H5Ovisit()` would previously internally traverse each object's path name from the iteration root group in order to retrieve information about that object, causing severe performance degradation with a deeply nested group structure. Modified the algorithm to instead retrieve information directly from the object. To get this benefit, users should use `H5Ovisit3()`, or use `H5Ovisit2()` with neither `H5O_INFO_HDR` nor `H5O_INFO_META_SIZE` selected in the `fields` parameter. Performance of `H5Ocopy()`, `H5Iget_name()`, and external links with a callback set should also improve in similar situations.

### Versioned API functions now default to earliest version for older API settings

   When a global API compatibility version is set (e.g., `H5_USE_16_API`), functions introduced after that version previously defaulted to their latest version, which could break applications. For example, an application using `H5_USE_16_API` that called `H5Sencode()` (introduced in 1.8, versioned in 1.12) would get `H5Sencode2()` instead of `H5Sencode1()`, potentially causing compilation or runtime failures. Versioned functions now default to their earliest (version 1) variant when the configured API level predates the function's introduction, providing maximum compatibility. See issue [#6278](https://github.com/HDFGroup/hdf5/issues/6278).

## Parallel Library

## Fortran Library

### Added Fortran bindings for the string-based filter configuration API

   - `h5pappend_filter_f` - generic interface with two overloads: string
     `params` variant and raw `cd_values` variant.
   - `h5pmodify_filter_by_idx_f` - replaces the configuration of the filter
     already at a given pipeline index, in place.
   - `h5pget_filter_params_by_idx_f` - retrieves the parameter string for a
     filter by pipeline index.
   - `h5zconfig_get_param_f` - generic interface dispatching to
     `h5zconfig_get_param_int_f`, `_double_f`, `_logical_f`, and `_str_f`
     based on the value argument type.
   - `h5zget_filter_info_class_f` - returns registry-level information about
     a filter into a new `h5z_class_info_f_t` derived type.

## C++ Library

### Added C++ wrappers for the string-based filter configuration API

   - `DSetCreatPropList::appendFilter` - two overloads: string `params` and
     raw `cd_values`.
   - `DSetCreatPropList::modifyFilterByIdx` - two overloads, replaces the
     configuration of the filter already at a given pipeline index, in
     place.
   - `DSetCreatPropList::getFilterParams` - retrieves a filter's parameter
     string by pipeline index.
   - `H5FilterParam::config_get_param` - four overloads dispatching on value
     type (`int64_t`, `double`, `bool`, `H5std_string`).

## Java Library

### Added Java wrappers for the string-based filter configuration API

   - `H5.H5Pappend_filter` - two overloads: `String params` and `int[] cd_values`.
   - `H5.H5Pmodify_filter_by_idx` - two overloads, replaces the
     configuration of the filter already at a given pipeline index, in
     place.
   - `H5.H5Pget_filter_params_by_idx` - retrieves a filter's parameter string
     by pipeline index.
   - `H5.H5Zconfig_get_param` - four overloads (`long[]`, `double[]`,
     `boolean[]`, `String[]` output arrays) providing typed parameter
     accessors equivalent to the C `H5Zconfig_get_int/double/bool/str`.
   - `H5.H5Zget_filter_class_info` - returns registry-level information
     about a filter into a new `H5Z_class_info_t` output object.

### Java dependency JAR paths are now user-configurable

   The CMake variables `HDF5_JAVA_LOGGING_JAR`, `HDF5_JAVA_LOGGING_NOP_JAR`, `HDF5_JAVA_LOGGING_SIMPLE_JAR`, `HDF5_JAVA_JUNIT_JAR`, and `HDF5_JAVA_HAMCREST_JAR` are now CMake cache variables with the bundled JARs as defaults. Users can override these at configure time to use system-provided JARs. See `INSTALL_CMake_options.md` for details.

## Tools

### h5dump: display string-based filter configuration (RFC-HDFG-2026-001)

   - `h5dump -p` now nests a `PARAMS_STRING` line inside each filter's
     `FILTERS` block, showing the filter's `key=value` configuration string
     when one is stored on disk.
   - A binary-exact floating-point value in `PARAMS_STRING` is annotated with
     its equivalent C99 hexadecimal-float spelling (e.g. `rate = 0.125 /*
     0x1p-3 */`) so the exact stored value is visible even when the decimal
     form is a truncated approximation.
   - `h5dump -p` also nests a `DESCRIPTION` line showing the filter's
     registry-sourced human-readable label, when the filter is currently
     registered and provides one. Because this is a best-effort, in-memory
     lookup rather than data recorded on disk, `DESCRIPTION` is omitted
     whenever the filter isn't currently registered, and its text can differ
     between machines with different plugin versions.

### h5repack: `UD=` string-form filter configuration

   - `h5repack -f` accepts a new string form of the `UD=` filter
     specification: `UD=filter_number,filter_flag,key=value[,key2=value2,...]`,
     alongside the existing `cd_value_count,value1[,value2,...]` integer
     form. For example: `h5repack -f UD=32013,0,rate="3.0" file1 file2`.

## High-Level APIs

## C Packet Table API

## Internal header file

## Documentation

   - Added a "Filter Pipeline Message - Version 3" section to the on-disk
     format specification (`H5.format.4.0.dox`), documenting the new object
     header message layout that stores a filter's `key=value` configuration
     string alongside its `cd_values`.
   - Added `DDLBNF300.dox`, the DDL grammar reference for the new
     `PARAMS_STRING`/`DESCRIPTION` display syntax introduced above.

# 🪲 Bug Fixes

## Library

### Fixed memory leaks and ID reference count issues when pushing an error to an error stack that is full

   When an error is pushed to an error stack, the library may make a copy of the file
   and function strings to ensure that they exist for the same duration as the error
   stack entry. When an error stack is full, the library simply makes any further pushes
   no-ops, but previously gave no information to calling code that this happened. This
   caused calling code to assume that the duplicated strings were owned by an error stack
   entry that was never pushed, leaking the duplicated strings. Additionally, IDs
   associated with the error stack entry were left with incremented reference counts,
   resulting in an infinite loop while closing the library.

### Library shutdown no longer aborts on a detected infinite loop

   When the library detects that it cannot make progress closing itself (an "infinite loop closing library"), it no longer calls `abort()`. The abort behaved inconsistently, only firing when automatic error message display was enabled. Additionally, terminating the entire host process on a shutdown-time condition is undesirable for applications that embed HDF5. The library now reports the condition (when error display is enabled) and returns without aborting.

   Fixes GitHub issue #6531

### Fixed a heap buffer overflow when decoding a shared message list

   When reading a shared object header message (SOHM) list from the metadata cache, `H5SM__cache_list_deserialize()` allocated the message array for `list_max` entries but drove the decode loop with the `num_messages` count read from the on-disk index header. A corrupted or malicious file whose `num_messages` exceeds `list_max` caused writes past the end of the array and reads past the end of the input buffer. The count is now validated against `list_max` before the loop runs.

### HTTP 403 errors in the ROS3 VFD for object keys with special characters

   The ROS3 VFD did not URI-encode the S3 object key when building the HTTP request path, so keys containing characters that AWS Signature Version 4 requires to be percent-encoded - such as the '=' in Hive-style `key=value` partition prefixes, '+', or spaces - produced a signed request whose signature did not match S3's server-side recomputation. S3 rejects such requests with `SignatureDoesNotMatch`, which surfaces as an HTTP 403 error (indistinguishable from a permissions error on a HEAD request), even though tools like the AWS CLI could access the same object. The object key is now percent-encoded exactly once when the request path is built, matching the behavior of other S3 clients. Note that URLs must now be passed to the ROS3 VFD with their object keys unencoded; a key that was pre-encoded as a workaround for this issue will now be double-encoded and fail to resolve.

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
### Fixed a crash when reading a chunked dataset whose chunk rank does not match the dataspace rank

   The chunk layout's stored dimensionality was validated against the dataspace rank at creation time, but not at open time, so a file whose stored chunk rank disagreed with its dataspace rank was not caught. The resulting inconsistent selection ranks during chunk I/O caused a divide-by-zero in the hyperslab iterator. The chunk dimensionality is now also validated on open, and such a dataset is rejected with an error instead of crashing.

   Fixes GitHub issue #6491

   Fixes CVE-2026-19025

## Java Library

## Configuration

### Fixed version handling in installed CMake package version configuration file

   The installed CMake package version configuration file for the library previously used `SameMinorVersion` for the version compatibility logic, causing a `find_package(HDF5 X.Y.Z)` call to fail unless the version of a located HDF5 installation matched both `X` and `Y` of the version number exactly (i.e., releases with a greater minor version number weren't considered backward compatible). This reflected the version compatibility of HDF5 releases prior to version 2.0.0, but doesn't reflect the version compatibility of HDF5 version 2.0.0+ releases. The version compatibility logic now uses `SameMajorVersion`, so a `find_package(HDF5 X.Y.Z)` call will accept all versions of HDF5 where the major version matches `X` (i.e., only releases with a greater major version number will be rejected as not backward compatible).

### Fixed the C++ examples failing to compile when built standalone

  The standalone examples build used C++98, but `H5public.h` includes
  `<cinttypes>`, which requires C++11. This affected any C++ translation unit
  including `hdf5.h`, and did not match the HDF5 C++ library itself, which is
  built as C++11. The C++ examples did not compile, against either static or
  shared HDF5. The examples are now built as C++11.

  Only the standalone build was affected. Examples built as part of the HDF5
  build inherit the library's own C++ standard.

### Fixed the examples skipping the HL, Fortran and C++ programs in some configurations

  When built standalone against an installed HDF5, the examples chose between
  the shared and static HL, Fortran and C++ libraries using `BUILD_SHARED_LIBS`,
  while the C library used `H5EXAMPLE_USE_SHARED_LIBS`. Since
  `H5EXAMPLE_USE_SHARED_LIBS` determines which component is requested from
  `find_package`, and therefore which `HDF5_<linkage>_<lang>_FOUND` variables
  exist, `BUILD_SHARED_LIBS` could not select a linkage on its own. With
  `H5EXAMPLE_USE_SHARED_LIBS` on and `BUILD_SHARED_LIBS` unset, those examples
  were disabled with a "libs not found" message even though the libraries were
  installed and had been found. The selection now uses
  `H5EXAMPLE_USE_SHARED_LIBS`, matching the C library.

  Builds driven through `CTestScript.cmake` were not affected, since its cache
  file forces `BUILD_SHARED_LIBS` on. This affected cases where the examples
  were built directly without that cache file.

## Tools

### Fixed an issue with quoting of data values in h5ls and h5dump when displaying as ASCII characters

   When using the `-s` (h5ls) or `-r` (h5dump) option to display 1-byte integer datasets and
   attributes as ASCII characters, a closing double-quote character for data values was dropped
   in some cases. This double-quote character has been restored and similar formatting issues
   have been fixed for cases where elements wrap to new lines according to the particular tool's
   column limit setting.

## Performance

## Fortran API

### h5open_f now re-initializes the Fortran interface after h5close_f

   An h5open_f / h5close_f / h5open_f sequence could leave the Fortran interface
   uninitialized. The second h5open_f reported success, but the predefined type
   handles were left holding identifiers that h5close_f had released, so later calls
   failed. Whether this happened depended on the Fortran compiler.

   Fixes GitHub issue #6642

### h5fget_obj_ids_f no longer returns the Fortran interface's own identifiers

   h5fget_obj_count_f excludes the objects h5open_f opens to represent the predefined
   types, but h5fget_obj_ids_f returned them, so the two disagreed about the same query
   and an application walking the list found datatypes it never opened. Both now report
   only what the application has open, matching the C API.

   Fixes GitHub issue #6648

### h5fget_obj_count_f and h5fget_obj_ids_f document their object type argument

   Both listed the object types as alternatives without mentioning that they may be
   combined with IOR(), which the C API supports and both have always passed through.

### h5fget_obj_count_f no longer returns negative counts

   With the Fortran interface open, counting a single object type across all files
   subtracted the objects opened by h5open_f, so queries for files, groups, and
   datasets returned a negative count and reported success. A negative count is now
   reported as an error.

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
   name was accepted - for example, a CLASS of `"IMAGE_EXTRA"` was treated as an IMAGE dataset, and
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

### Fortran test programs no longer exit successfully after a fatal error

   The Fortran tests ended unrecoverable failures with STOP, which exits with a
   success status, so a run that aborted part way through was reported as passing.

### New test for the object count and identifier list

   The Fortran tests had no coverage of h5fget_obj_ids_f over all files, and none that
   compared it against h5fget_obj_count_f. A new test opens objects of several types
   and checks that the two agree, that object types combined with IOR() count as the
   sum of their parts, and that a buffer shorter than the number of open objects is
   filled with the application's own.

### The h5open/h5close test checks that the interface re-initializes

   Its object counts were taken while the Fortran interface was closed, where no such
   call is permitted. They now run after the interface has been reopened, and confirm
   that the predefined types are usable again.

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
