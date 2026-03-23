HDF5 version 2.1.1 released on 2026-03-23

---

> [!Note]
>
> - A typo in variable LT_HL_F_VERS_INTERFACE left H5_HL_F_SOVERS_INTERFACE empty, resulting in malformed macOS linker flags. The typo was fixed in this patch release.
> - A change in the directory structure of one of the compression filters resulted in undetected failure signing the hdf5-2.1.0-macos14_clang.dmg release binary. This was found and fixed in this patch release.

---
HDF5 version 2.1.0 released on 2026-03-02

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
* [New Features & Improvements](CHANGELOG.md#-new-features--improvements)
* [Bug Fixes](CHANGELOG.md#-bug-fixes)
* [Support for new platforms and languages](CHANGELOG.md#-support-for-new-platforms-and-languages)
* [Platforms Tested](CHANGELOG.md#%EF%B8%8F-platforms-tested)
* [Known Problems](CHANGELOG.md#-known-problems)

# 🔆 Executive Summary: HDF5 Version 2.1.0

> [!IMPORTANT]
>
> - The format of the GitHub tag for HDF5 releases has been changed to Major.Minor.Patch, consistent with the versioning policy change to follow the Semantic Versioning Specification described in this [Wiki page](https://github.com/HDFGroup/hdf5/wiki/HDF5-Version-Numbers-and-Branch-Strategy).  The previous tag format hdf5_Major_Minor_Patch that was created in addition for HDF5 2.0.0 and this release will not be continued.

## Performance Enhancements:


## Significant Advancements:


## Enhanced Features:


## Java Enhancements:

  
## Acknowledgements: 

We would like to thank the many HDF5 community members who contributed to this release of HDF5.

# ⚠️ Breaking Changes


# 🚀 New Features & Improvements

## Configuration

### Added a CMake module to locate libaec for SZIP support

   A new `Findlibaec.cmake` CMake module has been added. This module is intended to locate libaec on the system for SZIP support in HDF5 when libaec was built with Autotools instead of CMake. When SZIP support is enabled in HDF5 with the `HDF5_ENABLE_SZIP_SUPPORT` option, this module will first check for an existing CMake-built libaec and use that if it's available. Otherwise, the module will heuristically search for libaec on the system. If necessary, the module can be hinted toward a particular libaec installation by setting the CMake variable `libaec_ROOT` to point to a directory. If it is known that a CMake-built libaec installation exists on the system in a non-standard location, the CMake variable `libaec_DIR` can instead be set to a directory containing a `libaec-config.cmake` file to cause the module to prefer that libaec installation.

### Refactored CMake library export mechanism

   The CMake build system now uses separate export targets for different library types: static libraries use `${HDF5_EXPORTED_TARGETS}_static`, Java libraries use `${HDF5_EXPORTED_TARGETS}_java`, and shared libraries continue using the main `${HDF5_EXPORTED_TARGETS}`. The generated `hdf5-config.cmake` file conditionally includes the appropriate target files (`hdf5-targets_static.cmake` and `hdf5-targets_java.cmake`), allowing downstream projects to selectively link against specific library variants. This refactoring improves build system maintainability and applies consistently across all HDF5 components (core, C++, Fortran, high-level APIs, tools, and Java JNI).

## Library

### Added predefined datatypes for FP6 data

   Predefined datatypes have been added for FP6 data in [E2M3 and E3M2 formats](https://www.opencompute.org/documents/ocp-microscaling-formats-mx-v1-0-spec-final-pdf).

   The following new macros have been added:

   - `H5T_FLOAT_F6E2M3`
   - `H5T_FLOAT_F6E3M2`

   These macros map to IDs of HDF5 datatypes representing a 6-bit floating-point datatype with 1 sign bit and either 2 exponent bits and 3 mantissa bits (E2M3 format) or 3 exponent bits and 2 mantissa bits (E3M2 format).

   Note that support for a native FP6 datatype has not been added yet. This means that any datatype conversions to/from the new FP6 datatypes will be emulated in software rather than potentially using specialized hardware instructions. Until support for a native FP6 type is added, an application can avoid datatype conversion performance issues if it is sure that the datatype used for in-memory data buffers matches one of the above floating-point formats. In this case, the application can specify one of the above macros for both the file datatype when creating a dataset or attribute and the memory datatype when performing I/O on the dataset or attribute.

   Also note that HDF5 currently has incomplete support for datatype conversions involving non-IEEE floating-point format datatypes. Refer to the 'Known Problems' section for information about datatype conversions with these new datatypes.

### Added predefined datatype for FP4 data

   A predefined datatype has been added for FP4 data in [E2M1 format](https://www.opencompute.org/documents/ocp-microscaling-formats-mx-v1-0-spec-final-pdf).

   The following new macro has been added:

   - `H5T_FLOAT_F4E2M1`

   This macro maps to the ID of an HDF5 datatype representing a 4-bit floating-point datatype with 1 sign bit, 2 exponent bits and 1 mantissa bit.

   Note that support for a native FP4 datatype has not been added yet. This means that any datatype conversions to/from the new FP4 datatype will be emulated in software rather than potentially using specialized hardware instructions. Until support for a native FP4 type is added, an application can avoid datatype conversion performance issues if it is sure that the datatype used for in-memory data buffers matches the above floating-point format. In this case, the application can specify the above macro for both the file datatype when creating a dataset or attribute and the memory datatype when performing I/O on the dataset or attribute.

   Also note that HDF5 currently has incomplete support for datatype conversions involving non-IEEE floating-point format datatypes. Refer to the 'Known Problems' section for information about datatype conversions with these new datatypes.

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

### Fixed a problem with using filters with variable length datatypes

   When using data filters with a dataset with a variable length datatype, the library would not invoke the filters' can_apply and set_local callbacks. This has been addressed and the library will now make those callbacks no matter what the datatype is.

### Fixed a potential out of bound read

   When a file is corrupted such that an array datatype's size, the number of elements, and the element size are not in agreement, it can trigger an out of bounds read.  A check has been added to detect such situation.

### Fixed security issue [CVE-2025-44904](https://www.cve.org/CVERecord?id=CVE-2025-44904)

   For unfiltered dataset chunks, the size on disk should be constant for all chunks in a dataset. In some cases the size of each chunk is stored even in this case where it can be inferred from the chunk dimensions and datatype. The code previously assumed this stored size was equal to the inferred size, leading to a mismatch in the expected and actual buffer size. Modified the library to throw an error if the size does not match the expected size.

### Fixed security issue [CVE-2025-2309](https://www.cve.org/CVERecord?id=CVE-2025-2309)

   Fixed a critical vulnerability in `H5T__bit_copy` of the component Type Conversion Logic which could lead to heap-based buffer overflow. 

### Fixed security issue [CVE-2025-2308](https://www.cve.org/CVERecord?id=CVE-2025-2308)

   Fixed a critical vulnerability in the function `H5Z__scaleoffset_decompress_one_byte` of the component Scale-Offset Filter which could lead to heap-based buffer overflow. 

### Fixed a double-free bug in `H5D__chunk_copy`

   Fixed a double-free bug in the internal `H5D__chunk_copy()` function which occurred when a buffer was re-allocated without updating the original pointer freed later on.

   Fixes GitHub issues [#6123](https://github.com/HDFGroup/hdf5/issues/6123)
                       [#6124](https://github.com/HDFGroup/hdf5/issues/6124)
                       [#6125](https://github.com/HDFGroup/hdf5/issues/6125)
                       [#6126](https://github.com/HDFGroup/hdf5/issues/6126)
                       [#6133](https://github.com/HDFGroup/hdf5/issues/6133)
### Fixes potential security issues

   The `get_name` API functions allow passing NULL when querying the object name length. However, passing a non-NULL buffer with size == 0 will result in security vulnerability of invalid write. That was because the library wrote a null terminator to the buffer regardless of what the size of the buffer was as long as the buffer was non-NULL.
   These functions are now fixed to treat (buffer != NULL, size == 0) as a length-only query to eliminate Valgrind error of invalid write.

### Fixed a performance issue with chunked dataset I/O

   When dataset chunks are unable to be placed in the dataset chunk cache (for example, if a chunk is too large), the library falls back to an alternative approach for I/O on dataset chunks. An issue with the logic in this approach prevented chunked dataset I/O from making use of the library's data sieve buffer I/O optimization functionality. For chunk shapes that are non-contiguous with the memory layout of a buffer, this could result in severely degraded I/O performance, with the worst-case behavior causing I/O to be performed on a single data element at a time.

   The data sieve buffer functionality has been extended to cover the case of uncached chunks and will be used as long as the underlying Virtual File Driver supports data sieving.

## Java Library

## Configuration

### Removed force-setting of `ZLIB_USE_EXTERNAL` and `SZIP_USE_EXTERNAL` CMake variables to `ON`

   When the CMake variable `HDF5_ALLOW_EXTERNAL_SUPPORT` is set to `GIT` or `TGZ`, the library's build process previously force-set the `ZLIB_USE_EXTERNAL` and `SZIP_USE_EXTERNAL` variables to `ON`. This prevented the ability to independently choose whether zlib and szip are built from system libraries or from external sources. These variables are no longer forced to `ON` in this case and can be set individually.


## Tools

## Performance

## Fortran API

### Added Fortran wrappers for SWMR functionality

   Added four new Fortran wrappers that provide direct access to SWMR (Single Writer Multiple Reader) C APIs:
   - `h5fstart_swmr_write_f` - Enables SWMR writing mode for a file
   - `h5dflush_f`            - Flushes dataset buffers to disk
   - `h5pset_append_flush_f` - Sets append flush property values including optional callback function
   - `h5pget_append_flush_f` - Retrieves append flush property values including callback function

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

    `H5T_FLOAT_F8E4M3`<br>
    `H5T_FLOAT_F8E5M2`<br>
    `H5T_FLOAT_F6E2M3`<br>
    `H5T_FLOAT_F6E3M2`<br>
    `H5T_FLOAT_F4E2M1`

   If possible, an application should perform I/O with these datatypes using an in-memory type that matches the specific floating-point format and perform explicit data conversion outside of HDF5, if necessary. Otherwise, read/written values should be verified to be correct.

- When the library detects and builds in support for the _Float16 datatype, an issue has been observed on at least one MacOS 14 system where the library fails to initialize due to not being able to detect the byte order of the _Float16 type [#4310](https://github.com/HDFGroup/hdf5/issues/4310):

     #5: H5Tinit_float.c line 308 in `H5T__fix_order()`: failed to detect byte order
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
