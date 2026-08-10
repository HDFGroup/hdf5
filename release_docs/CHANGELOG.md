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

### The installed CMake package now prefers the shared libraries when no linkage is requested

   A `find_package(HDF5 NAMES hdf5 CONFIG)` call that names neither the `static` nor the `shared` component previously resolved to the static libraries of an installation providing both, and only fell back to shared when no static libraries were installed. That preference is now reversed: shared is used when available, with static as the fallback. Setting `HDF5_USE_STATIC_LIBRARIES` before the call still selects the static libraries, matching the behavior of CMake's `FindHDF5` module.

   The preference decides which `HDF5_<lang>_<LINKAGE>_LIBRARY` variables and Fortran module directory the call sets, as well as the linkage the new `hdf5::` targets refer to, so a project that relied on the previous default and did not name a linkage will now link the shared libraries. Such a project should request the `static` component explicitly or set `HDF5_USE_STATIC_LIBRARIES`. Calls that already name a `static` or `shared` component are unaffected.


# 🪦 Deprecations

### Deprecated the linkage-qualified CMake target names

   The `hdf5-shared` / `hdf5-static` targets, their `_hl`, `_cpp`, `_hl_cpp`, `_fortran` and `_hl_fortran` counterparts, and the `HDF5_<lang>_<LINKAGE>_LIBRARY` variables are deprecated in favor of the stable `hdf5::` target names described under Configuration below. They will be removed in a future major release, so new projects should link `hdf5::hdf5` and friends, and existing projects should migrate to the name names when convenient. See `docs/USING_HDF5_CMake.md` for the mapping from old names to new ones.


# 🚀 New Features & Improvements

## Configuration

### Added stable, linkage-agnostic CMake target names to the installed package

   Downstream projects can now link `hdf5::hdf5`, `hdf5::hdf5_hl`, `hdf5::hdf5_cpp`, `hdf5::hdf5_hl_cpp`, `hdf5::hdf5_fortran` and `hdf5::hdf5_hl_fortran` instead of the linkage-qualified `hdf5-shared` and `hdf5-static` targets, along with an `HDF5::HDF5` aggregate and `hdf5::<tool>` for each installed tool. The names do not encode whether the library is static or shared, so the same project file works against a static installation, a shared one, or one providing both. These are the names CMake's own `FindHDF5` module provides, so a project using them no longer needs different code depending on whether HDF5 was located through `FindHDF5` or through HDF5's `hdf5-config.cmake`, and they are defined identically when HDF5 is built as a subproject with `add_subdirectory()`. Which linkage the targets refer to is chosen while resolving the package: by a `static` or `shared` component, by `HDF5_USE_STATIC_LIBRARIES`, or by default shared-if-available and static otherwise, which is the same order the rest of the package resolution now follows (see Breaking Changes). The existing `hdf5-shared` / `hdf5-static` targets and the `HDF5_<lang>_<LINKAGE>_LIBRARY` variables are otherwise unchanged, and the public names are aliases onto those same targets, so a project naming both on its link line still gets a single library.


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
