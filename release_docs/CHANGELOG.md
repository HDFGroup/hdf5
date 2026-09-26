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

- When a `find_package (HDF5 ...)` call within a CMake project uses HDF5's `hdf5-config.cmake`
  configuration file (a Config mode search), requesting both "shared" and "static" components
  simultaneously will now fail. Only one of the "shared" or "static" components should be requested
  when locating HDF5. Consequently, the `HDF5_LIB_TYPE` CMake variable set by the configuration file
  will only be set to one of "shared" or "static", depending on the requested library type, rather
  than potentially being a list of both. For the time being, both sets of HDF5's "-shared" and
  "-static" CMake targets will continue to be available after the `find_package (HDF5 ...)` call,
  regardless of which library type was requested.

- When a `find_package (HDF5 ...)` call within a CMake project uses HDF5's `hdf5-config.cmake`
  configuration file (a Config mode search), the consuming project may now be required to have
  one or more CMake languages enabled, depending on the specific COMPONENTS requested. HDF5's
  configuration file previously enabled these languages automatically with calls to
  `enable_language()`, but these calls were removed in favor of checking the enabled languages
  and issuing an error if required languages aren't enabled.

# 🪦 Deprecations


# 🚀 New Features & Improvements

## Configuration

### Various improvements in installed CMake package configuration file

   - Fixed `find_dependency()` calls so that `PRIVATE`-linked libraries are only propagated as
     transitive link requirements for static library targets (Fixes GitHub issue #6347)
   - Added missing `find_dependency()` calls for some `PRIVATE`-linked libraries
   - Fixed an issue where `find_package()` for parallel-enabled HDF5 installations may fail when
     trying to locate MPI Fortran support, even if HDF5 Fortran support isn't requested (Fixes
     GitHub issue #6366)
   - Fixed an issue where the `HDF5_LIB_TYPE` CMake variable would be undefined if some HDF5
     components were requested in a `find_package()` call, but "shared" or "static" was not requested
   - Removed a call to `enable_language()` in favor of checking the currently enabled CMake languages
     and failing if a required language isn't enabled
   - Added a CMake variable for the enabled/disabled status of the "digitally signed plugins"
     feature
   - Fixed the CMake variable for the enabled/disabled status of the `HDF5_DIMENSION_SCALES_NEW_REF`
     option
   - Reduced the scope of some temporary variables and modifications so they don't propagate to
     consuming CMake projects

## Library

### Added typed accessors for filter parameter strings

   Filter plugins can read `key = value` parameter strings with the new `H5Zconfig_has_key()`, `H5Zconfig_get_int()`, `H5Zconfig_get_double()`, `H5Zconfig_get_bool()` and `H5Zconfig_get_str()` functions, declared in `H5Zdevelop.h`. The strings use a subset of TOML v1.0.0 syntax, and hex-float literals such as `0x1.8p+1` are accepted and read back bit-for-bit. These functions do not take the library's API lock, so they can be called from inside a filter callback that runs while the lock is held. This is the first part of the string-based filter configuration API (RFC-HDFG-2026-001).

   Two third-party libraries are now compiled into libhdf5: the [tomlc17](https://github.com/cktan/tomlc17) TOML parser (MIT license) in `src/tomlc17/`, and the [Ryu](https://github.com/ulfjack/ryu) shortest round-trip float formatter (Boost/Apache-2.0 license) in `src/ryu/`. Their symbols are hidden in the shared library and renamed in the static library, so they cannot collide with an application's own copies.

### Added the H5F_LIBVER_V300 library version bound

   The `H5F_libver_t` enumeration gains `H5F_LIBVER_V300` for the 3.0 file format, and `H5F_LIBVER_LATEST` now maps to it. Every object header message version admitted by `H5F_LIBVER_V300` is the same as for `H5F_LIBVER_V200` except the filter pipeline message, which admits version 3 (see below); later format changes in the 3.0 release will also be gated on it. The constant is also available in the Fortran (`H5F_LIBVER_V300_F`) and Java (`HDF5Constants.H5F_LIBVER_V300`) bindings, and `h5repack --low`/`--high` accept the value 6.

### Added the H5Z_class3_t filter class and H5Zget_filter_class_info()

   Filters can now be registered with the new `H5Z_class3_t` structure, declared in `H5Zdevelop.h` with its `version` field set to `H5Z_CLASS3_T_VERS`. A class3 filter carries a required canonical `name` (1-255 bytes from `[A-Za-z0-9_.-]`, unique among registered class3 filters), an optional free-form `description`, and optional `set_config`/`get_config` callbacks for string-based configuration. Its filter callback has the new `H5Z_func2_t` signature, which adds the data transfer property list, the chunk's scaled coordinates and the dataset rank to the arguments of `H5Z_func_t`. `H5Z_func2_t` also has a `void *state` parameter that is reserved for future per-dataset filter state; the library always passes NULL for it in this release. The built-in deflate, shuffle, Fletcher32, N-bit, scale-offset and szip filters are now registered as class3 filters.

   `H5Z_class_t` still selects `H5Z_class2_t` by default; an application can map it to `H5Z_class3_t` by defining `H5Z_class_t_vers` to 3. `H5Zregister()` accepts all three class versions.

   The new `H5Zget_filter_class_info()` function returns a registered filter's encode/decode configuration flags, canonical name and description in an `H5Z_class_info_t` structure, along with whether the filter provides `set_config` and `get_config` callbacks. Like `H5Zfilter_avail()`, it loads the filter plugin if the filter is not yet registered.

### Changed the filter name reported for filters with no known name

   `H5Pget_filter2()` and `H5Pget_filter_by_id2()` now return the decimal filter ID as a string (for example, `"32000"`) as the name of a filter that has no name stored in the pipeline and is not registered. They returned `"Unknown library filter"` for such a filter with an ID below 256, and an empty string otherwise.

### Added string-based filter configuration to dataset creation property lists

   The new `H5Pappend_filter()` function appends a filter to a pipeline from an `H5Z_params_t` descriptor, declared in `H5Zpublic.h`. With `H5Z_PARAMS_RAW(n, cd_values)` it behaves like `H5Pset_filter()`; with `H5Z_PARAMS_STR("level = 6")` the library loads the filter if necessary and calls its `set_config` callback to translate the `key = value` string into `cd_values`. The string is canonicalized (outer braces stripped, hex-float literals rewritten to the shortest decimal that round-trips) and stored on the pipeline entry together with the resulting `cd_values`, and the filter's canonical name is recorded in the entry. `H5Pmodify_filter_by_idx()` replaces the configuration of the filter at a given pipeline index, in either form, without changing the filter order. `H5Pget_filter_params_by_idx()` returns the parameter string of the filter at a given index: the stored string if there is one, otherwise the filter's `get_config` reconstruction from `cd_values`, otherwise a `cd_values=v0:v1:...` listing. `H5Pmodify_filter()` clears a stored string, since the new `cd_values` no longer match it.

   Stored strings are carried by `H5Pcopy()` and by `H5Pencode()`/`H5Pdecode()`, and are written to files in version 3 of the filter pipeline message (see below), so the creation property list of a dataset opened from a file returns the stored string.

   The built-in filters now implement `set_config`: deflate accepts `level` (0-9, default 6), szip accepts `coding` (`"nn"` or `"entropy"`) and `pixels_per_block`, and scale-offset requires `scale_type` (`"int"`, `"float_dscale"` or `"float_escale"`) and `scale_factor`. Shuffle, Fletcher32 and N-bit accept only an empty string. Deflate, szip and scale-offset also implement `get_config`.

   An `H5Pencode()` buffer for a dataset creation property list in which some filter carries a parameter string uses an extended encoding of the filter pipeline property that earlier releases reject when decoding. Property lists without parameter strings encode exactly as before, and buffers produced by earlier releases decode as before.

### Added version 3 of the filter pipeline message

   Version 3 of the filter pipeline object header message stores each filter's configuration string after its client data values, as a 2-byte length followed by the string bytes without a NUL terminator; a zero length means the filter has no string. The library writes version 3 for a dataset whose pipeline carries at least one configuration string when the file's high library version bound is `H5F_LIBVER_V300` or later. If the high bound is lower, `H5Dcreate()`, `H5Dcreate_anon()` and `H5Ocopy()` fail with a minor error code of `H5E_BADRANGE` rather than write the pipeline without its strings. `H5Ocopy()` carries the strings to the copy, and `H5Pget_filter_params_by_idx()` returns them for a dataset opened from a file without loading the filter.

   Pipelines without configuration strings are written as version 1 or 2 as before, so a file that stores no strings is byte-identical to one written by earlier releases when the low bound is below `H5F_LIBVER_V300`. Because `H5F_LIBVER_V300` and `H5F_LIBVER_LATEST` admit version 3, a low bound of either writes every filter pipeline message as version 3, even one without strings, and HDF5 2.x and earlier releases cannot open the datasets in such a file. The version 3 layout is described in the file format specification.

### Added support for internally concurrent multithreaded reads of chunked datasets

   Added 3 new functions to support this: H5TSset_internal_threads(),
   H5Pset_io_threads(), and H5Pget_io_threads().

   This feature internally parallelizes read operations on chunked datasets.
   H5TSset_internal_threads() is used to enable the feature globally, while
   H5Pset_io_threads() can be used to disable the feature on a per-operation
   basis. These functions are only available when the library is configured with
   HDF5_ENABLE_CONCURRENCY=ON. When performing an internally threaded read, the
   library will concurrently read from disk, unfilter, and scatter to memory all
   chunks in a read operation on a chunked dataset. Currently each of these
   sub-operations is serialized (protected by a mutex), so there is not yet
   likely to be any performance improvement.

## Parallel Library

## Fortran Library

### Added Fortran wrappers for string-based filter configuration

   `h5pappend_filter_f` and `h5pmodify_filter_by_idx_f` add or replace a filter from either a `key=value` parameter string or a `cd_values` array (generic interfaces), and `h5pget_filter_params_by_idx_f` returns a filter's parameter string. They wrap `H5Pappend_filter()`, `H5Pmodify_filter_by_idx()` and `H5Pget_filter_params_by_idx()`. The string is passed as a Fortran character variable; the wrapper adds the C terminator.

## C++ Library

## Java Library

## Tools

### h5repack -f accepts filter configuration strings

   The `UD=` filter specification of `h5repack -f` has a string form, `UD=filter_number,filter_flag,key=value[,key=value...]`, in addition to the `cd_value_count,value1[,value2...]` form. The string goes to the filter's `set_config` callback through `H5Pappend_filter()`, and the filter plugin must implement `set_config`. For example: `h5repack -f UD=32013,0,rate="3.0" file1 file2`. A third field containing `=` selects the string form.

### h5dump -p shows each filter's configuration string

   With `-p`/`--properties`, h5dump now prints a `PARAMS_STRING` line inside each filter's entry in the `FILTERS` block. It shows the configuration string stored in the file, or else the filter's `get_config` form, or else a `cd_values=` listing. When the string contains a float that is a small multiple of a power of two, a following `# key = <hex float>` line gives its exact value; this line is outside the quoted string and is display-only. A `DESCRIPTION` line gives the filter's registered description when the filter is available on the machine running h5dump, so it can differ between machines.

   This changes the layout of `h5dump -p` output for every filtered dataset: filters that printed on one line, such as `COMPRESSION DEFLATE { LEVEL 9 }` or `PREPROCESSING SHUFFLE`, now print as multi-line blocks. Scripts that parse `h5dump -p` output may need updating. The grammar is documented in the new DDL in BNF for HDF5 3.0.0 page.

## High-Level APIs

## C Packet Table API

## Internal header file

## Documentation


# 🪲 Bug Fixes

## Library

### Fixed a deadlock in the ROS3 VFD on Windows

   When an HDF5 application running on Windows and using the ROS3 VFD exited normally,
   a deadlock would occur when the VFD called the aws-c-s3 library's cleanup function
   during process shutdown. This was due to the aws-c-s3 library attempting to join
   threads while the Windows loader lock was held. As a temporary workaround for Windows
   builds of the library, the aws-c-s3 cleanup logic has been moved to the VFD's
   termination callback (other platforms still use an atexit() handler) and will be
   skipped if the VFD determines that the process is being shutdown. Due to the current
   architecture of the library, the aws-c-s3 library's resources can only be properly
   cleaned up if the HDF5 application makes sure to call H5close() before exiting.
   Otherwise, memory leaks and other resource cleanup issues may be observed.

   Fixes GitHub issue #6560

### Fixed a heap buffer overflow when decoding object header messages

   The size stored in an object header message header was checked against the chunk before the rest of that message header was decoded, allowing a message body to start up to four bytes further into the chunk than the check accounted for. A corrupted or fuzzed file could declare a size that passed the check and still extended past the end of the chunk image, and the message's decode callback was then handed a buffer end outside the allocation. `H5O__chunk_deserialize()` now checks the message size once the whole message header has been decoded.

   Fixes GitHub issue #6401

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

### Fixed crashes when reading datasets with malformed N-Bit or Fletcher32 filter metadata

   Reading a dataset from a corrupted or maliciously crafted file could crash the library in the N-Bit and Fletcher32 filter decode paths. The N-Bit filter dereferenced its client-data parameter array before validating it, crashing when the array was empty or NULL, and walked the compressed chunk during decompression without bounding the input against the chunk size, causing out-of-bounds reads. It also indexed that parameter array at offsets taken from the datatype description held in the array itself, without bounding those offsets against the number of parameters supplied, so a parameter list stopping short of the datatype it described was read past its end. The Fletcher32 filter subtracted the 4-byte checksum length from the chunk size without checking that the chunk was at least that large, underflowing the length passed to the checksum routine. These filters now validate their parameters and buffer sizes and fail with an error instead of crashing.

   Fixes GitHub issues #6488, #6489, #6490, and #6492

   Fixes CVE-2026-19026, CVE-2026-19027, and CVE-2026-19028

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
