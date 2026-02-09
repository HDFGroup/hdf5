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
* [New Features & Improvements](CHANGELOG.md#-new-features--improvements)
* [Bug Fixes](CHANGELOG.md#-bug-fixes)
* [Support for new platforms and languages](CHANGELOG.md#-support-for-new-platforms-and-languages)
* [Platforms Tested](CHANGELOG.md#%EF%B8%8F-platforms-tested)
* [Known Problems](CHANGELOG.md#-known-problems)

# 🔆 Executive Summary: HDF5 Version 2.2.0

## Performance Enhancements:


## Significant Advancements:


## Updated Foundation:

> [!IMPORTANT]
>
> - Transitioned to [CMake-only](CHANGELOG.md#cmake) builds, and Autotools is no longer in use.
> - Renamed library state variables, notably `HDF5_ENABLE_PARALLEL` is now `HDF5_PROVIDES_PARALLEL`, see PR [#5716](https://github.com/HDFGroup/hdf5/pull/5716) for more details.
> - The default setting for `H5Fset_libver_bounds` has been updated to set the lower bound to the HDF5 library version 1.8. This change ensures that users can take advantage of the library's optimal performance and the latest features by default. If users need their files to be compatible with older versions of the HDF5 library, they will need to adjust this lower bound manually.
> - The format of the GitHub tag for HDF5 releases has been changed to Major.Minor.Patch, consistent with the versioning policy change to follow the Semantic Versioning Specification described in this [Wiki page](https://github.com/HDFGroup/hdf5/wiki/HDF5-Version-Numbers-and-Branch-Strategy).  The previous tag format hdf5_Major_Minor_Patch that was created in addition for the 2.0.0 and 2.1.0 releases will not be continued.   

## Enhanced Features:


## Java Enhancements:

  
## Acknowledgements: 

We would like to thank the many HDF5 community members who contributed to this release of HDF5.

# ⚠️ Breaking Changes


# 🚀 New Features & Improvements

## Configuration

### Added a CMake module to locate libaec for SZIP support

   A new `Findlibaec.cmake` CMake module has been added. This module is intended to locate libaec on the system for SZIP support in HDF5 when libaec was built with Autotools instead of CMake. When SZIP support is enabled in HDF5 with the `HDF5_ENABLE_SZIP_SUPPORT` option, this module will first check for an existing CMake-built libaec and use that if it's available. Otherwise, the module will heuristically search for libaec on the system. If necessary, the module can be hinted toward a particular libaec installation by setting the CMake variable `libaec_ROOT` to point to a directory. If it is known that a CMake-built libaec installation exists on the system in a non-standard location, the CMake variable `libaec_DIR` can instead be set to a directory containing a `libaec-config.cmake` file to cause the module to prefer that libaec installation.

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

## Java Library

## Configuration

## Tools

## Performance

## Fortran API

### Added Fortran wrappers for SWMR functionality

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
