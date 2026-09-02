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

### Fixed a crash when reading a chunked dataset whose chunk rank does not match the dataspace rank

   The chunk layout's stored dimensionality was validated against the dataspace rank at creation time, but not at open time, so a file whose stored chunk rank disagreed with its dataspace rank was not caught. The resulting inconsistent selection ranks during chunk I/O caused a divide-by-zero in the hyperslab iterator. The chunk dimensionality is now also validated on open, and such a dataset is rejected with an error instead of crashing.

   Fixes GitHub issue #6491

   Fixes CVE-2026-19025

## Java Library

### Fixed datatype ID leaks when reading or writing nested datatypes through the JNI

   The object-tree read and write helpers in the JNI derived a base datatype from the memory type with `H5Tget_super()` for the variable-length, array and complex classes, but never closed it. Because an `hid_t` is not reclaimed when a native method returns, every read or write of such data leaked at least one datatype ID for the lifetime of the process, and a nested type leaked one per level. The helpers now close the derived type on both the success and error paths.

   Fixes GitHub issue #6592

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
