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

## Parallel Library

## Fortran Library

## C++ Library

## Java Library

## Tools

## High-Level APIs

## C Packet Table API

## Internal header file

## Documentation


# 🪲 Bug Fixes

## Library

### Fixed error when reading variable-length chunked datasets in read-only mode

   When reading from a chunked dataset with a variable-length type, a non-default fill value, and unwritten chunks, the library would internally try to write data to the file and fail due to writing to a read-only file. Reworked the I/O code to avoid these writes in this case. This may also improve performance and file space usage in similar cases with files open with write access.

### Validate free space section type during decode

   When loading a free space section info block, the per-section type byte read from the file was used directly to index the free space manager's section class array and to call the class `deserialize` callback, guarded only by an assertion that is removed in release builds. A corrupted or fuzzed file could supply a type beyond the number of registered classes, causing an out-of-bounds read of the class array and an indirect call through a bogus function pointer. `H5FS__cache_sinfo_deserialize()` now rejects a section type that is not less than the number of section classes.

### Fixed a heap buffer overflow when decoding a shared message list

   When reading a shared object header message (SOHM) list from the metadata cache, `H5SM__cache_list_deserialize()` allocated the message array for `list_max` entries but drove the decode loop with the `num_messages` count read from the on-disk index header. A corrupted or malicious file whose `num_messages` exceeds `list_max` caused writes past the end of the array and reads past the end of the input buffer. The count is now validated against `list_max` before the loop runs.

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

### Library shutdown no longer aborts on a detected infinite loop

   When the library detects that it cannot make progress closing itself (an "infinite loop closing library"), it no longer calls `abort()`. The abort behaved inconsistently, only firing when automatic error message display was enabled. Additionally, terminating the entire host process on a shutdown-time condition is undesirable for applications that embed HDF5. The library now reports the condition (when error display is enabled) and returns without aborting.

### Hardened decoding of serialized dataspace selections against malformed buffers

   `H5S_select_deserialize()` and the per-selection-type deserialize callbacks (all, hyperslab, none, and point) previously computed the pointer to the last valid buffer byte as `buffer + size - 1` without first checking the buffer size. A buffer shorter than the 4-byte selection-type header, or a zero-length selection-info buffer, would underflow this computation and produce an out-of-bounds end pointer, defeating subsequent overflow checks. The deserialize routines now reject a buffer that is too small to hold the selection type, and they reject an empty selection-info buffer before deriving the end pointer. Hyperslab decoding additionally now rejects a serialized rank of 0 or greater than `H5S_MAX_RANK`. As a companion fix, `H5S__hyper_serialize()` now returns an error when asked to serialize a hyperslab selection on a rank-0 (scalar or null) dataspace, a state that can arise when a dataspace extent is collapsed to a scalar after a hyperslab selection has already been made.

## Java Library

## Configuration

### Fixed version handling in installed CMake package version configuration file

   The installed CMake package version configuration file for the library previously used `SameMinorVersion` for the version compatibility logic, causing a `find_package(HDF5 X.Y.Z)` call to fail unless the version of a located HDF5 installation matched both `X` and `Y` of the version number exactly (i.e., releases with a greater minor version number weren't considered backward compatible). This reflected the version compatibility of HDF5 releases prior to version 2.0.0, but doesn't reflect the version compatibility of HDF5 version 2.0.0+ releases. The version compatibility logic now uses `SameMajorVersion`, so a `find_package(HDF5 X.Y.Z)` call will accept all versions of HDF5 where the major version matches `X` (i.e., only releases with a greater major version number will be rejected as not backward compatible).

## Tools

## Performance

## Fortran API

## High-Level Library

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
