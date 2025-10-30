HDF5 version 2.0.0-4 currently under development

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
* [Executive Summary](CHANGELOG.md#-executive-summary-hdf5-version-200)
* [Breaking Changes](CHANGELOG.md#%EF%B8%8F-breaking-changes)
* [New Features & Improvements](CHANGELOG.md#-new-features--improvements)
* [Bug Fixes](CHANGELOG.md#-bug-fixes)
* [Support for new platforms and languages](CHANGELOG.md#-support-for-new-platforms-and-languages)
* [Platforms Tested](CHANGELOG.md#%EF%B8%8F-platforms-tested)
* [Known Problems](CHANGELOG.md#-known-problems)

# 🔆 Executive Summary: HDF5 Version 2.0.0

## Performance Enhancements:

- Up to [2500% faster](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#rtree) Virtual Dataset read/write operations
- [30% faster opening](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#layoutcopydelay) and [25% faster closing](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#fileformat) of virtual datasets.
- [Reduced memory overhead](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#fileformat) via shared name strings and optimized spatial search algorithms for virtual datasets.

## Significant Advancements:

- Full [UTF-8](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#utf-8) filename support on Windows, resolving encoding issues from previous versions.
- Introduction of bfloat16 predefined datatypes for efficient machine learning conversions.
- First-class support for [complex numbers](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#complex), eliminating manual workarounds in scientific applications.

## Updated Foundation:

- New [file format](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#fileformat) version (4.0) and compliance with the C11 standard.

> [!IMPORTANT]
>
> - Transitioned to [CMake-only](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#cmake) builds, and Autotools is no longer in use.
> - Renamed library state variables, notably `HDF5_ENABLE_PARALLEL` is now `HDF5_PROVIDES_PARALLEL`, see PR [#5716](https://github.com/HDFGroup/hdf5/pull/5716) for more details.
> - The default setting for `H5Fset_libver_bounds` has been updated to set the lower bound to the HDF5 library version 1.8. This change ensures that users can take advantage of the library's optimal performance and the latest features by default. If users need their files to be compatible with older versions of the HDF5 library, they will need to adjust this lower bound manually.

## Enhanced Features:

- Improved [ROS3 VFD](https://github.com/HDFGroup/hdf5/blob/develop/release_docs/CHANGELOG.md#ros3) capabilities using the aws-c-s3 library.

# ⚠️ Breaking Changes

### Updated default file format to 1.8

   By default, HDF5 will now use the 1.8 file format (`H5F_LIBVER_V18`). This provides improved performance and space efficiency, particularly with groups and links. However, HDF5 library versions 1.6 and earlier will not be able to read files created with the default settings. The previous behavior can be restored using `H5Pset_libver_bounds(fapl_id, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST)`.

### Renamed the option: `HDF5_ENABLE_Z_LIB_SUPPORT`

   The option has been renamed to `HDF5_ENABLE_ZLIB_SUPPORT` to be consistent with the naming of other options. Also, the option defaults to OFF. This requires the user to explicitly enable zlib support when configuring the library.

### Autotools support was removed from HDF5<a name="cmake">

   CMake is now the build system available in HDF5 code. Version 3.26 or later is required. See the AutotoolsToCMakeOptions.md file for highlights of the CMake HDF5 install layout and CMake options to use in place of former Autotools options.

### Fixed problems with family driver and user block

   When using a user block with the family driver, the driver would inappropriately subtract the user block size for each member file when calculating member EOAs. This could cause a failure when an address overflowed the calculated eoa. The driver would also add the user block size when returning the EOF. Modified the family driver to not consider the user block, as it is handled by the H5FD layer. The user block now spans the first X bytes of the family array, for example a 4 KiB user block with 3 KiB member size will take up the entire first member and the first 1 KiB of the second. This may cause compatibility issues with preexisting family files with user blocks, though the way it worked before was inconsistent if it worked at all.

# 🚀 New Features & Improvements

## Configuration

### Improved the cross-compile support in the build system

   The CMake build system has been improved to better support cross-compiling. This includes the following changes:
   - The CMake option, `CMAKE_CROSS_COMPILING`, indicates that the library is being cross-compiled. This option, usually in a toolchain file, can be set to ON when cross-compiling.
   - Removed the `CMAKE_CROSSCOMPILING_EMULATOR` when cross-compiling, CMake will automatically insert it in the command.
   - Added an option `HDF5_USE_PREGEN` to supply pre-generated files located in `HDF5_USE_PREGEN_DIR` directory, to bypass running feature detection programs when cross-compiling.
   - Added a variable `CROSSCOMPILING_PATH` to specify a path to search for programs when cross-compiling. This is useful when the build system needs to run programs that were built for the host system.

### Refactored `HDF5_BUILD/ENABLE_{feature}` variable in hdf5-config.cmake file

   The variables used in hdf5-config.cmake to indicate what options were used to build the installed library have been renamed. All `HDF5_BUILD/ENABLE_{feature}` variables are now `HDF5_PROVIDES_{feature}`. This more clearly indicates that these variables reflect whether the feature is supported by the installed library, instead of whether the feature is an option that can be changed when building an application with the library.

   Created MACRO `EXTERNAL_HDF5_STATUS` to convert between the old and new names. The macro is in the config/examples/HDF5SubdirMacros.cmake file and can be copied into a project's CMakeLists.txt file to provide backward compatibility.

### CMake minimum version is now 3.26

   The minimum version of CMake is now 3.26; this will remove workarounds to handle versions between 3.18 (the previous minimum) and 3.26.

### Removed `HDF5_ENABLE_THREADS` option

   The `HDF5_ENABLE_THREADS` option has been removed, as it no longer functions as a proper build option. The library will always check for thread support and set the internal status variable, `HDF5_THREADS_ENABLED`. The `HDF5_ENABLE_THREADSAFE` option is still available to build with thread-safe API calls.

 - Reorganized the files in the config/cmake folder into the config folder structure

   The config folder CMake files have been reorganized to make it easier to maintain and add new features. This includes the following changes:
   - The files in the config folder are the macros and templates for the build process.
   - The files in the config/cmake folder are primarily for optional features.
   - The files in the config/install folder are for installation support.
   - The files in the config/flags folder are for compiler support.
   - The remaining folders remain unchanged.

### Added CMake configuration options to override compilers in h5cc:

   | | |
   | --- | --- |
   | `HDF5_H5CC_C_COMPILER` | for the C compiler |
   | `HDF5_H5CC_CXX_COMPILER` | for the C++ compiler |
   | `HDF5_H5CC_Fortran_COMPILER` | for the Fortran compiler |

   These default to the currently used compiler, preserving the current behavior. However, they can be overridden by users who need to use a different compiler at runtime, for example, when they build via cache.

### Added `CMAKE_INSTALL_PREFIX` to the default plugin path

   To help users find their plugins, the default plugin path has been changed to include the `CMAKE_INSTALL_PREFIX`. Adding the install prefix allows users to skip setting the `HDF5_PLUGIN_PATH` environment variable when using plugins with the default lib/plugin location.

### Converted documentation in the source folder, doc, to doxygen files.

### Added configuration option for API concurrency support:

   CMake: `HDF5_ENABLE_CONCURRENCY` (ON/OFF) (Default: OFF)

   This option enables support for concurrent multithreaded operation of supported API routines. This option also provides thread-safe execution of all other, non-concurrent operations. The 'concurrency' option thus is a superset of the existing 'threadsafe' option. Both options are currently available, although mutually exclusive. As the 'concurrency' code becomes more stable over time, the 'threadsafe' option may be deprecated in favor of the new 'concurrency' option.

   The following API routines support concurrent multithreaded operation:
   <none yet>

### Added support for MinGW + MSYS2 when building with CMake

   We added support for this to the appropriate configure-time checks in CMake. CMake + MinGW + MSYS2 is now tested with the following environments:
   - mingw32
   - mingw64
   - ucrt64
   - clang64

### Added CMake build mode flags to the libhdf5.settings file

   Flags from the CMake build mode (e.g., optimization) are not a part of `CMAKE_<language>_FLAGS` and were not exported to the libhdf5.settings file. This has been fixed, and the C, Fortran, and C++ build mode flags are now exported to the file.

   This also affects the text output of `H5check_version()` and the libhdf5.settings string stored in the library (for those who use strings(1), etc. to get build info from the binary).

### CMake: Split compiler-specific flags into separate files

   The compiler-specific flags have been split into separate files to make it easier to maintain and add new compiler flags. The flags for NVHPC, Intel, GNU and Clang compilers are now in separate files included from the current compiler flags files; `HDFCompiler<language>Flags.cmake`.

### Added support for native zlib-ng compression

Changed the zlib-ng CMake logic to prefer the native zlib-ng library. Added `#ifdef` around the compression function calls. Added including the correct header file with the same `#ifdef`.

### Renamed HDF5Examples build options from `H5EXAMPLE_<option>` to `H5EXAMPLE_<option>`

Changed the prefix to better distinguish the examples build options from the library options when building the examples along with the library.

### Renamed remaining HDF5 library CMake options except for CMake BUILD* variables

| Old | New |
| --- | --- |
| `DEFAULT_API_VERSION` | `HDF5_DEFAULT_API_VERSION` |
| `DISABLE_PDB_FILES` | `HDF5_DISABLE_PDB_FILES` |
| `ONLY_SHARED_LIBS` |  `HDF5_ONLY_SHARED_LIBS` |
| `ALLOW_UNSUPPORTED` | `HDF5_ALLOW_UNSUPPORTED` |
| `TEST_SHELL_SCRIPTS` | `HDF5_TEST_SHELL_SCRIPTS` |

All other HDF5 library CMake options are prefixed with `HDF5_`

### bin/cmakehdf5 has been removed

   This was an unsupported build script that made building HDF5 via CMake work like building HDF5 via the Autotools. It has been unmaintained for a long time, has been marked deprecated, and is being removed.

### Generated files in src are now checked into version control

   These files are infrequently updated, and generating them adds a dependency on Perl. The listed files are now checked in and do not need to be recreated when checking out development branches.
   - H5Edefin.h
   - H5Einit.h
   - H5Emajdef.h
   - H5Emindef.h
   - H5Epubgen.h
   - H5Eterm.h
   - H5overflow.h
   - H5version.h

### Dropped some old Solaris Studio work-arounds

   Solaris Studio no longer seems to be maintained and the last version (12.4, circa 2015) doesn't seem to fully support C11. We've removed some hacks that work around things like `__attribute__()` support.

### Dropped support for the traditional MSVC preprocessor

   Visual Studio has [recently started using a standards-compliant preprocessor](https://learn.microsoft.com/en-us/cpp/preprocessor/preprocessor-experimental-overview?view=msvc-170) (In VS2019+) and this is the default in C11. Because of this, we've dropped support for the traditional MSVC preprocessor.

   The standard for building the library is now C11. We have updated the build files to set the C standard to C11, though some platforms use gnu11 to get some GNU things to work.

## Library

### Updated default file format to 1.8

   By default, HDF5 will now use the 1.8 file format (`H5F_LIBVER_V18`). This provides improved performance and space efficiency, particularly with groups and links. This behavior can be overridden with `H5Pset_libver_bounds()`.

### Added predefined datatypes for bfloat16 data

   Predefined datatypes have been added for little- and big-endian bfloat16 (https://en.wikipedia.org/wiki/Bfloat16_floating-point_format) data.

   The following new macros have been added:

    - H5T_FLOAT_BFLOAT16LE / H5T_FLOAT_BFLOAT16BE

      These macros map to IDs of HDF5 datatypes representing a little- or big-endian 16-bit floating-point datatype with 1 sign bit, 8 exponent bits and 7 fraction bits.

      Note that support for a native bfloat16 datatype has not been added yet. This means that any datatype conversions to/from the new bfloat16 datatypes will be emulated in software rather than potentially using specialized hardware instructions. Until support for a native bfloat16 type is added, an application can avoid datatype conversion performance issues if it is sure that the datatype used for in-memory data buffers matches the above floating-point format (such as the __bf16 type). In this case, the application can specify one of the above macros for both the file datatype when creating a dataset or attribute and the memory datatype when performing I/O on the dataset or attribute.

### Removed hbool_t from public API calls
                                  
### Removed `hbool_t` from public API calls
                                  
The `hbool_t` type was introduced before the library supported C99's Boolean type. Originally typedef'd to an integer, it has been typedef'd to C99's bool for many years.

It had been previously purged from the bulk of the library code and only remained in public API signatures. In HDF5 2.0, it has also been removed from public API signatures.

The `hbool_t` typedef remains in H5public.h so existing code does not need to be updated.

### `H5public.h` no longer includes `features.h`

`features.h` is supposed to be included by glibc headers and not used in application code. It is unnecessary given our use of feature test macros like `_POSIX_C_SOURCE` and has been removed.

### Improved performance of opening a virtual dataset with many mappings

   When opening a virtual dataset, the library would previously decode the mappings in the object header package, then copy them to the dataset struct, then copy them to the internal dataset creation property list. Copying the VDS mappings could be very expensive if there were many mappings. Changed this to delay decoding the mappings until the dataset code, and delay copying the layout to the DCPL until it is needed. This results in only the decoding and no copies in most use cases, as opposed to the decoding and two copies with the previous code.

### Aligned the CMake compiler wrappers with the old Autotools versions

The versions of `h5cc`, `h5fc`, `h5c++`, etc. generated by CMake were missing several options and features from the Autotools counterparts. Some of these options and features have now been implemented in the CMake versions, while some of them have not:
- The missing `--help`/`-h`, `-c`, `-echo`, and `-shlib`/`-noshlib` options have been implemented.
- The `-prefix` option was not implemented, as it didn't appear to function in the Autotools wrappers and is generally covered by pkg-config in the CMake wrappers.
- A new `-nohl` option has been added to avoid building and linking against the high-level HDF5 libraries if desired.
- Similar to the Autotools wrappers, the CMake wrappers now add the HDF5 installation library directory to the rpath of the resulting executable/library by default when linking against shared HDF5 libraries. This behavior can be avoided by specifying the new `-norpath` option.
- Parsing of the `HDF5_USE_SHLIB` environment variable has been added to determine whether to link against shared or static HDF5 libraries. Precedence is still given to the `-shlib`/`-noshlib` options.
- Parsing of the `HDF5_PKG_CONFIG_ARGS` environment variable has been added to separate pkg-config-specific options from compiler-specific options and prevent conflicts between them.

Several issues were also fixed in the pkg-config files that are generated by CMake.

### Changed the default page buffer size for the ROS3 driver

Calling `H5Pset_fapl_ros3()` now has the side effect of setting the page buffer size in the FAPL to 64 MiB if it was not previously set. This will only have an effect if the file uses paged allocation. Also added the `H5F_PAGE_BUFFER_SIZE_DEFAULT` to allow the user to unset the page buffer size in an FAPL so it can be similarly overridden.

### Default dataset chunk cache size increased

   The default dataset chunk cache size was increased to 8 MiB (8,388,608 bytes).

### The file format has been updated to 4.0<a name="fileformat"></a>

The Virtual Dataset Global Heap Block format has been updated to version 1 to support shared string storage for source filenames and dataset names, reducing file size when multiple mappings reference the same sources. This new format is only used when the HDF5 library version bounds lower bound is set to 2.0 or later.

Use of the shared strings option for Virtual Datasets reduces memory overhead and optimizes dataset close operations.

The chunked dataset file format has been updated to always use 64 bits to encode the size of filtered chunks. This will allow data filters that expand the chunks by a large amount to still work. Chunk sizes are still limited to `2^32 - 1`. This new format is only used when the HDF5 library version bounds lower bound is set to 2.0 or later.

### The `H5Dread_chunk()` signature has changed

A new parameter, `nalloc`, has been added to `H5Dread_chunk()`. This parameter contains a pointer to a variable that holds the size of the buffer buf. If *nalloc is not large enough to hold the entire chunk being read, no data is read. On exit, the value of this variable is set to the buffer size needed to read the chunk.

The old signature has been renamed to `H5Dread_chunk1()` and is considered deprecated:
```c
   herr_t H5Dread_chunk1(hid_t dset_id, hid_t dxpl_id,
                         const hsize_t *offset, uint32_t *filters,
                         void *buf);
```

The new signature is `H5Dread_chunk2()`. All code should be updated to use this version:
```c
   herr_t H5Dread_chunk2(hid_t dset_id, hid_t dxpl_id,
                         const hsize_t *offset, uint32_t *filters,
                         void *buf, size_t *nalloc);
```

`H5Dread_chunk()` will map to the new signature unless the library is explicitly configured to use an older version of the API.

### Replaced the ROS3 VFD's S3 backend based on libcurl with a new backend based on the [aws-c-s3 library](https://github.com/awslabs/aws-c-s3)<a name="ros3"></a>

The ROS3 VFD now requires the `aws-c-s3` library in order to be built. This library offers several useful features for the VFD, including the following:
   - Automatic retries of non-fatal failed requests (where the libcurl backend would simply return an error),
   - Built-in sourcing of credentials and other configuration information from standard AWS configuration files and environment variables,
   - Automatic splitting and parallelization of large S3 requests,
   - Built-in handling of the formation of HTTP request authentication headers (where the libcurl backend had to manage this manually), and more.

Additional benefits with the rewrite of this backend include:
   - Support for `s3://` object URIs,
   - The ability to enable debugging information printouts at runtime by setting a new environment variable `HDF5_ROS3_VFD_DEBUG` (refer to `H5FDros3.h`),
   - The ability to capture logging information from the `aws-c-s3` library by setting a new environment variable `HDF5_ROS3_VFD_LOG_LEVEL` (refer to `H5FDros3.h`).

With these changes, the behavior of the `authenticate` field of the ROS3 VFD's FAPL structure has changed slightly. If `authenticate` is true, the ROS3 VFD will _only_ use credentials specified in the FAPL structure and will not attempt to load credentials from other places. In this case, the `secret_id` and `secret_key` fields must still both be non-empty strings, or an error will be returned when opening a file. If a session token is to be used, it must be specified with `H5Pset_fapl_ros3_token()`.

If `authenticate` is false, the ROS3 VFD will instead attempt to load credentials from several different places, in this order:
   - From the environment, by checking AWS environment variables such as `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN` and `AWS_ACCOUNT_ID`,
   - From the AWS profile files, by reading from `~/.aws/config` and `~/.aws/credentials`, by default. The specific files read from can be overridden with the `AWS_CONFIG_FILE` and `AWS_SHARED_CREDENTIALS_FILE` environment variables,
   - From STS, by using `AssumeRoleWithWebIdentity`,
   - From EC2 instance metadata.

If the ROS3 VFD cannot source credentials from any of these locations, it will fall back to using anonymous credentials.

This functionality effectively deprecates the `--s3-cred` option for the `h5dump`, `h5ls`, and `h5stat` tools. However, this option has been kept for compatibility reasons and can still be used to force the specified credentials to take precedence over any credentials that the VFD would otherwise try to source.

Note that with these changes, the AWS region to be used must always be specified. However, the region can now be specified in several ways other than just in the FAPL structure. The VFD will search for a specified AWS region in the following order:
   - The FAPL, if `aws_region` is not an empty string
   - The `AWS_REGION` environment variable
   - The `AWS_DEFAULT_REGION` environment variable
   - The AWS configuration file (`~/.aws/config` by default)
   - The `default` profile from this file is used, unless a different profile is specified with the AWS_PROFILE environment variable

If the ROS3 VFD cannot determine an AWS region from one of these locations, an error will be returned when opening a file.

New API functions `H5Pset_fapl_ros3_endpoint()` and `H5Pget_fapl_ros3_endpoint()` have been added for use with the ROS3 VFD. These functions set/get an alternate endpoint URL to use when opening files with the ROS3 VFD. This is useful in cases where the application needs to access files that are in a location other than the standard `s3.<region-code>.amazonaws.com`, which is what the ROS3 VFD uses when an alternate endpoint URL isn't specified. The ROS3 VFD also checks the `AWS_ENDPOINT_URL_S3` and `AWS_ENDPOINT_URL` environment variables for an alternate endpoint URL if one isn't specified with `H5Pset_fapl_ros3_endpoint()`.

Instructions for building the ROS3 VFD with the `aws-c-s3` library are in the [INSTALL_S3.txt](./INSTALL_S3.txt) file. The ROS3 VFD and information about the usage of the driver are described in the HDF5 user's guide.

### Renamed some API decorations

Some API decorations (used to hide `__declspec` on Windows, among other things) have been renamed:

| Old | New |
| --- | --- |
| `H5_DLLCPP(VAR)` | `H5CPP_DLL(VAR)` |
| `H5_HLDLL(VAR)` | `H5HL_DLL(VAR)` |
| `H5_HLCPPDLL(VAR)` | `H5CPP_HL_DLL(VAR)` |
| `H5_FCDLL(VAR)` | `H5FC_DLL(VAR)` |
| `H5_FCTESTDLL(VAR)` | `H5FC_TEST_DLL(VAR)` |
| `HDF5_HL_F90CSTUBDLL(VAR)` | `H5FC_HL_DLL(VAR)` |

### The `H5Iregister_type()` signature has changed

The hash_size parameter has not been used since early versions of HDF5 1.8, so it has been removed, and the API call has been versioned.

The old signature has been renamed to `H5Iregister_type1()` and is considered deprecated:

```c
H5I_type_t H5Iregister_type1(size_t hash_size, unsigned reserved, H5I_free_t free_func);
```

The new signature is `H5Iregister_type2()`. New code should use this version:

```c
H5I_type_t H5Iregister_type2(unsigned reserved, H5I_free_t free_func);
```

`H5Iregister_type()` will map to the new signature unless the library is explicitly configured to use an older version of the API.

### The `H5Tdecode()` signature has changed

   When provided malformed or too-small buffers, `H5Tdecode()` would crash. The new buffer size parameter allows this to be reliably avoided.

   The old signature has been renamed to `H5Tdecode1()` and is considered deprecated:

      hid_t H5Tdecode1(const void *buf);

   The new signature is `H5Tdecode2()`. New code should use this version:

      hid_t H5Tdecode2(const void *buf, size_t buf_size);

   `H5Tdecode()` will map to the new signature unless the library is explicitly configured to use an older version of the API.

### `H5F_LIBVER_LATEST` is now an enum value

This was previously #defined to the latest `H5F_libver_t API` version, but is now an enum value with an integer value equal to the latest `H5F_libver_t` API version's value. e.g.:

```
      <snip>
      H5F_LIBVER_V200 = 5,
      H5F_LIBVER_LATEST = 5,
      </snip>
```
### Added support for complex number datatypes<a name="complex"></a>

Support for the C99 `float _Complex`, `double _Complex` and `long double _Complex` (with MSVC, `_Fcomplex`, `_Dcomplex` and `_Lcomplex`) types has been added for platforms/compilers that support them. These types have been implemented with a new datatype class, `H5T_COMPLEX`. Note that any datatypes of class H5T_COMPLEX will not be readable with previous versions of HDF5. If a file is accessed with a library version bounds "high" setting less than `H5F_LIBVER_V200`, an error will occur if the application tries to create an object with a complex number datatype. If compatibility with previous versions of HDF5 is desired, applications should instead consider adopting [one of the existing conventions](https://nc-complex.readthedocs.io/en/latest/#conventions-used-in-applications).

The following new macros have been added:

| Macro | Description |
| --- | --- |
| `H5_HAVE_COMPLEX_NUMBERS` | This macro is defined in `H5pubconf.h` and will have the value 1 if native support for complex numbers is available. It will not be defined otherwise. |
| `H5_HAVE_C99_COMPLEX_NUMBERS` | This macro is defined in `H5pubconf.h` and will have the value 1 if native support for C99 complex numbers is available. It will not be defined otherwise. If this macro is not defined but `H5_HAVE_COMPLEX_NUMBERS` is defined, the complex number types supported are the MSVC types. |
| `H5_SIZEOF_FLOAT_COMPLEX` | This macro is defined in `H5pubconf.h` and will have a value corresponding to the size of the native float complex datatype, as computed by sizeof(). If C99 complex number support is available, this will be the size of the "float _Complex" type. Otherwise, it will be the size of the `_Fcomplex` type. It will have the value 0 if support for a native float complex datatype is not available. |
| `H5_SIZEOF_DOUBLE_COMPLEX` | This macro is defined in `H5pubconf.h` and will have a value corresponding to the size of the native double complex datatype, as computed by sizeof(). If C99 complex number support is available, this will be the size of the `double _Complex` type. Otherwise, it will be the size of the `_Dcomplex` type. It will have the value 0 if support for a native double complex datatype is not available. |
| `H5_SIZEOF_LONG_DOUBLE_COMPLEX` | This macro is defined in `H5pubconf.h` and will have a value corresponding to the size of the native long double complex datatype, as computed by sizeof(). If C99 complex number support is available, this will be the size of the `long double _Complex` type. Otherwise, it will be the size of the `_Lcomplex` type. It will have the value 0 if support for a native long double complex datatype is not available. |
| ``H5T_NATIVE_FLOAT_COMPLEX`` | This macro maps to the ID of an HDF5 datatype representing the native C float complex datatype (either "float _Complex" or `_Fcomplex`) for the platform. If support for a native float complex datatype is not available (`H5_HAVE_COMPLEX_NUMBERS` is not defined), the macro will map to `H5I_INVALID_HID` and should not be used. |
| ``H5T_NATIVE_DOUBLE_COMPLEX`` | This macro maps to the ID of an HDF5 datatype representing the native C double complex datatype (either `double _Complex` or `_Dcomplex`) for the platform. If support for a native double complex datatype is not available (`H5_HAVE_COMPLEX_NUMBERS` is not defined), the macro will map to `H5I_INVALID_HID` and should not be used. |
| ``H5T_NATIVE_LDOUBLE_COMPLEX`` | This macro maps to the ID of an HDF5 datatype representing the native C long double complex datatype (either `long double _Complex` or `_Lcomplex`) for the platform. If support for a native long double complex datatype is not available (`H5_HAVE_COMPLEX_NUMBERS` is not defined), the macro will map to `H5I_INVALID_HID` and should not be used. |
| `H5T_COMPLEX_IEEE_F16LE` / `H5T_COMPLEX_IEEE_F16BE` | These macros map to IDs of HDF5 datatypes representing a complex number of two parts, each of which is an IEEE 754 16-bit floating-point datatype in little- or big-endian order. These datatypes are available regardless of whether complex number support is available or not. |
| `H5T_COMPLEX_IEEE_F32LE` / `H5T_COMPLEX_IEEE_F32BE` | These macros map to IDs of HDF5 datatypes representing a complex number of two parts, each of which is an IEEE 754 32-bit floating-point datatype in little- or big-endian order. These datatypes are available regardless of whether complex number support is available or not. |
| `H5T_COMPLEX_IEEE_F64LE` / `H5T_COMPLEX_IEEE_F64BE` | These macros map to IDs of HDF5 datatypes representing a complex number of two parts, each of which is an IEEE 754 64-bit floating-point datatype in little- or big-endian order. These datatypes are available regardless of whether complex number support is available or not. |

The following new API function has been added:

| Function | Description |
| --- | --- |
| `hid_t H5Tcomplex_create(hid_t base_type_id)` | Creates a new complex number datatype from the base datatype specified by the given HDF5 ID `base_type_id`. The base datatype must be a floating-point datatype. |

The following new hard datatype conversion paths have been added, but will only be used when complex number support is available:

| | |
| -- | -- |
| `H5T_NATIVE_SCHAR`   <-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_UCHAR`   <-> `H5T_NATIVE_FLOAT_COMPLEX` |
| `H5T_NATIVE_SHORT`   <-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_USHORT`  <-> `H5T_NATIVE_FLOAT_COMPLEX` |
| `H5T_NATIVE_INT` <-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_UINT`<-> `H5T_NATIVE_FLOAT_COMPLEX` |
| `H5T_NATIVE_LONG`<-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_ULONG`   <-> `H5T_NATIVE_FLOAT_COMPLEX` |
| `H5T_NATIVE_LLONG`   <-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_ULLONG`  <-> `H5T_NATIVE_FLOAT_COMPLEX` |
| `H5T_NATIVE_FLOAT16` <-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_FLOAT`   <-> `H5T_NATIVE_FLOAT_COMPLEX` |
| `H5T_NATIVE_DOUBLE`  <-> `H5T_NATIVE_FLOAT_COMPLEX` | `H5T_NATIVE_LDOUBLE` <-> `H5T_NATIVE_FLOAT_COMPLEX` |
| | |
| `H5T_NATIVE_SCHAR`   <-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_UCHAR`   <-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| `H5T_NATIVE_SHORT`   <-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_USHORT`  <-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| `H5T_NATIVE_INT` <-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_UINT`<-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| `H5T_NATIVE_LONG`<-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_ULONG`   <-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| `H5T_NATIVE_LLONG`   <-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_ULLONG`  <-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| `H5T_NATIVE_FLOAT16` <-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_FLOAT`   <-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| `H5T_NATIVE_DOUBLE`  <-> `H5T_NATIVE_DOUBLE_COMPLEX` | `H5T_NATIVE_LDOUBLE` <-> `H5T_NATIVE_DOUBLE_COMPLEX` |
| | |
| `H5T_NATIVE_SCHAR`   <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_UCHAR`   <-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| `H5T_NATIVE_SHORT`   <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_USHORT`  <-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| `H5T_NATIVE_INT` <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_UINT`<-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| `H5T_NATIVE_LONG` <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_ULONG`   <-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| `H5T_NATIVE_LLONG`   <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_ULLONG`  <-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| `H5T_NATIVE_FLOAT16` <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_FLOAT`   <-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| `H5T_NATIVE_DOUBLE`  <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | `H5T_NATIVE_LDOUBLE` <-> `H5T_NATIVE_LDOUBLE_COMPLEX` |
| | |
| `H5T_NATIVE_FLOAT_COMPLEX`  <-> `H5T_NATIVE_DOUBLE_COMPLEX` |  |
| `H5T_NATIVE_FLOAT_COMPLEX`  <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | |
| `H5T_NATIVE_DOUBLE_COMPLEX` <-> `H5T_NATIVE_LDOUBLE_COMPLEX` | |

Alternative software implementation conversion paths have been added for all of the above for use when native complex number support is not available. All of these conversion paths follow the behavior outlined in the C standard for conversions of complex number values.

Additionally, a special datatype conversion path has been added between complex number datatypes and array or compound datatypes, where the in-memory layout of data is the same between the datatypes and data can be directly converted. This conversion path is subject to the following rules:

- An array datatype must consist of exactly two elements, where each element is of the same floating-point datatype as the complex number datatype's base floating-point datatype.
- A compound datatype must consist of exactly two fields, where each field is of the same floating-point datatype as the complex number datatype's base floating-point datatype. The compound datatype must not have any leading or trailing structure padding or any padding between its two fields. The fields must also have compatible names, must have compatible offsets within the datatype and must be in the order of "real" part -> "imaginary" part, such that the compound datatype matches the following representation:

```
H5T_COMPOUND {
<float_type> "r(e)(a)(l)";  OFFSET 0
<float_type> "i(m)(a)(g)(i)(n)(a)(r)(y)"; OFFSET SIZEOF("r(e)(a)(l)")
}
```

where `r(e)(a)(l)` means the field may be named any substring of "real", such as "r", or "re", and `i(m)(a)(g)(i)(n)(a)(r)(y)` means the field may be named any substring of "imaginary", such as "im" or "imag".

Support for complex numbers has been added to the `h5dump`, `h5ls`, and `h5diff`/`ph5diff` tools. The `h5dump` command-line option '-m' can be used to change the floating-point printing format for the float complex and double complex datatypes, as well as the long double complex datatype if it has the same size as a double complex datatype.

Support for the predefined complex number datatypes and the `H5Tcomplex_create` function has been added to the Java wrappers. However, Java does not have official types for complex numbers, so an application must be sure that data is in an appropriate format in-memory when using these datatypes.

Support for the Fortran wrappers has not yet been added.

Support for the predefined complex number datatypes and the `H5Tcomplex_create` function has been added to the high-level library, allowing them to work with the `H5LTtext_to_dtype` and `H5LTdtype_to_text` functions.

Simple example programs showing how to use complex number datatypes have been added in the following files:

- `HDF5Examples/C/H5T/200/h5ex_t_complex.c`   (Uses C99 complex number types)
- `HDF5Examples/C/H5T/200/h5ex_t_complex_msvc.c`   (Uses MSVC complex number types)
- `HDF5Examples/C/H5T/200/h5ex_t_complex_custom.c` (Uses `H5Tcomplex_create()` to create a custom complex number type)

### FOR VOL DEVELOPERS: Renamed `H5VLstart_lib_state` and `H5VLfinish_lib_state`

   The APIs `H5VLstart_lib_state` and `H5VLfinish_lib_state` have been renamed to `H5VLopen_lib_context` and `H5VLclose_lib_context`, respectively, with the addition of a "context" argument.

### Removed `H5FDperform_init` API routine.

   Virtual File Driver (VFD) developers who wish to provide an ID for their driver should create a routine specific to their individual implementation.

### `H5Pset_external()` now uses `HDoff_t`, which is always a 64-bit type

   The `H5Pset_external()` call took an off_t parameter in HDF5 1.14.x and earlier. On POSIX systems, off_t is specified as a 64-bit type via POSIX large-file support (LFS). On Windows, however, off_t is defined as a 32-bit type, even on 64-bit Windows.

   HDoff_t has been added to H5public.h and is defined to be int64_t on Windows and the library has been updated to use HDoff_t in place of off_t throughout. The H5Pset_external() offset parameter has also been updated to be HDoff_t.

   There is no API compatibility wrapper for this change.

   Fixes GitHub issue [#3506](https://github.com/HDFGroup/hdf5/issues/3506)

### `H5Pset*` routines now fail when used on default property lists

   Modifying default property lists was never fully supported and could produce inconsistent and unexpected behavior.

### `H5Pset_vol()` now fails when used on a non-file-access property list

   Similar to the above. Setting the connector on a non-FAPL had no effect on library behavior, and the connector ID and information could not be read back from that plist later.

### Optimized Virtual Dataset opens by delaying layout copy<a name="layoutcopydelay"></a>

   On dataset open, the dataset performed an internal copy of the layout in order to populate its internal DCPL. For virtual datasets, this added a significant amount of overhead to the open operation.

   This layout copy is now delayed until either a user requests the DCPL, or until the start of an operation that needs to read the layout from the DCPL.

### Virtual datasets now use a spatial tree to optimize searches<a name="rtree"></a>

   Virtual dataset operations with many (>1,000) mappings were much slower than
   corresponding operations on normal datasets. This was due to the need
   to iterate through every source dataset's dataspace and check for an intersection
   with the user-selected region for a read/write in the virtual dataset.

   Virtual datasets with many mappings now use an r-tree (defined in H5RT.c) to
   perform a spatial search. This allows the dataspaces that intersect the
   user-selection to be computed with, in most cases, much fewer intersection checks,
   improving the speed of VDS read/write operations.

   Virtual datasets will use the r-tree by default, since the majority of use cases,
   should see improvements from use of the tree. However, because some workflows may
   find that the overhead of the tree outweighs the time saved on searches, there is
   a new Dataset Access Property List (DAPL) property to control use of the spatial tree.

   This property can be set or queried with the new API functions
   H5Pset_virtual_spatial_tree()/H5Pget_virtual_spatial_tree().

## Parallel Library

### Added H5FDsubfiling_get_file_mapping() API function for subfiling VFD

Added H5FDsubfiling_get_file_mapping() API function to retrieve the names of all physical subfiles that collectively make up a logical HDF5 file when using the subfiling Virtual File Driver.

## Fortran Library

### Added Fortran wrapper h5fdsubfiling_get_file_mapping_f() for subfiling VFD

Added Fortran wrapper h5fdsubfiling_get_file_mapping_f() for the subfiling file mapping functionality, ensuring complete language binding support.


## C++ Library

## Java Library

## Tools

### Added AWS endpoint command option to allow specifying an alternate endpoint URL when using the ROS3 VFD

   The new option is --endpoint-url, which allows the user to set an alternate endpoint URL other than the standard "protocol://service-code.region-code.amazonaws.com". If "--endpoint-url" is not specified, the ROS3 VFD will first check the AWS_ENDPOINT_URL_S3 and AWS_ENDPOINT_URL environment variables for an alternate endpoint URL before using a default one, with the region-code being supplied by the FAPL or standard AWS locations/environment variables.

   This option is supported by the following tools:
      `h5dump`, `h5ls`, `h5stat`

### Specifying ROS3 VFD on the command line is not required when using S3 URI

   If using an S3 URI to reference an HDF5 file in S3 (example: s3://mybucket/myfile.h5), then ROS3 VFD will be automatically selected unless the command-line option for the virtual file driver is used.

   This feature applies to the following tools: `h5dump`, `h5ls`, `h5stat`.

### Deprecated `h5dump` XML option

   The `h5dump` XML option is deprecated and will be removed in a future release. The XML output format has not been maintained and is not up-to-date with the latest features of HDF5.

### Added `h5dump` command option to set the floating point format for long double

   The new option is --lformat, which allows the user to set the floating point format for long double. The default format is %Lg. There is already an option --format to set the floating point format for double and float. The default format is %g.

### Removed the high-level GIF tools

   The high-level GIF tools, `h52gif` and `gif2h5`, have unfixed CVE issues (with no proof-of-concept files). They are not critical tools, are not well-maintained, and are an odd fit for building with the library. Because of this, they have been removed. We may move them to a separate repository in the future.

   This also removed the following configure options:
      CMake: `HDF5_BUILD_HL_GIF_TOOLS`

## High-Level APIs

## C Packet Table API

## Internal header file

## Documentation

### The COPYING file has been renamed to LICENSE

   This is where most people will expect to find license information. The COPYING_LBNL_HDF5 file has also been renamed to LICENSE_LBNL_HDF5. The licenses are unchanged.

# 🪲 Bug Fixes

## Library

### Fixed problems with family driver and user block

   When using a user block with the family driver, the driver would inappropriately subtract the user block size for each member file when calculating member EOAs. This could cause a failure when an address overflowed the calculated eoa. The driver would also add the user block size when returning the EOF. Modified the family driver to not consider the user block, as it is handled by the H5FD layer. The user block now spans the first X bytes of the family array, for example a 4 KiB user block with 3 KiB member size will take up the entire first member and the first 1 KiB of the second. This may cause compatibility issues with preexisting family files with user blocks, though the way it worked before was inconsistent if it worked at all.

### Fixed security issue CVE-2025-7067

   Fixed a heap buffer overflow in H5FS__sinfo_serialize_node_cb() by discarding file free space sections from the file free space manager when they are found to be invalid. Specifically crafted HDF5 files can result in an attempt to insert duplicate or overlapping file free space sections into a file free space manager, later resulting in a buffer overflow when the same free space section is serialized to the file multiple times.

   Fixes GitHub issue #5577

### Fixed security issue CVE-2025-2915 and OSV-2024-381

   Fixed a heap-based buffer overflow in H5F__accum_free caused by an integer overflow when calculating new_accum_size. Added validation in H5O__mdci_decode to detect and reject invalid values early, preventing the overflow condition.

   Fixes GitHub issue #5380
  
### Fixed security issue CVE-2025-7068

   Failures during the discard process on a metadata cache entry could cause the library to skip calling the callback to free the cache entry. This could result in resource leaks and issues with flushing and closing the metadata cache during file close. This has been fixed by noting errors during the discard process, but attempting to fully free a cache entry before signalling that an error has occurred.

   Fixes GitHub issue #5578

### Fix bugs in object header operations

   In some rare circumstances, such as deleting hard links that point to their own parent group in a file using the new file format, memory corruption could occur due to recursive operations changing data structures being operated on by multiple levels of recursion. Made changes to delay changing the data structure in a dangerous way until recursion is complete.

   Fixes GitHub issue #5854

### Fixed security issues CVE-2025-6816, CVE-2025-6856 and CVE-2025-2923

   A specially constructed HDF5 file could contain a corrupted object header with a continuation message that points back to itself. This could result in an internal buffer being allocated with too small of a size, leading to a heap buffer overflow. This has been fixed by checking the expected number of object header chunks against the actual value as chunks are being deserialized.

   Fixes GitHub issues #5571, #5574 and #5381

### Fixed security issue CVE-2025-6750

   A heap buffer overflow occurred because an mtime message was not properly decoded, resulting in a buffer of size 0 being passed into the encoder.  This has been fixed by decoding old and new mtime messages which will allow invalid message size to be detected.

   Fixes GitHub issue #5549

### Fixed CVE-2025-6269

   There were several security vulnerabilities found in the function H5C__reconstruct_cache_entry(), including buffer overflows and memory leaks.  The function has been hardened with bounds checks, input validation, and safe cleanup.

   Fixes GitHub issues #5579 and #5581

### Fixed a problem with the scale-offset filter

   A security fix added to 1.14.6 introduced a regression where certain data values could trigger a library error (not a crash or segfault).

   Fixes GitHub issue #5861

### Fixed security issue CVE-2025-2153

   The message flags field could be modified such that a message that is not sharable according to the share_flags field in H5O_msg_class_t can be treated as sharable. An assert has been added in H5O__msg_write_real to make sure messages that are not sharable can't be modified to shared. Additionally, the check in H5O__chunk_deserialize that catches unsharable messages being marked as sharable has been improved.

   Fixes GitHub issue #5329

### Fixed security issue CVE-2025-2925
   Actual_len + H5C_IMAGE_EXTRA_SPACE, which was used by H5MM_realloc as the size input, could equal 0 due to bad inputs. When H5MM_realloc was called, it freed image, but then could get sent to done before new_image could be assigned to image. Because the pointer for image wasn't null, it was freed again in done, causing a double-free vulnerability. H5C__load_entry() now checks for an image buffer length of 0 before calling H5MM_realloc.

   Fixes Github issue #5383

### Fixed security issue CVE-2025-6857

   An HDF5 file had a corrupted v1 B-tree that would result in a stack overflow when performing a lookup on it. This has been fixed with additional integrity checks.

   Fixes GitHub issue #5575

### Check for overflow in decoded heap block addresses

   Currently, we do not check for overflow when decoding addresses from the heap, which can cause overflow problems. We've added a check in H5HL__fl_deserialize to ensure no overflow can occur.

   Fixes GitHub issue [#5382](https://github.com/HDFGroup/hdf5/issues/5382)

### Revised handling of Unicode filenames on Windows<a name="utf-8">

   In the HDF5 1.14.4 release, a change was made to address some issues with the library's handling of code pages and file paths on Windows.  This change introduced other issues with the handling of UTF-8 file names that caused breakage for software using the 1.14.4 and 1.14.5 releases of HDF5. That change was reverted for the 1.14.6 release and the behavior has been slightly modified for this release.

   On Windows, the library once again assumes that filename strings will be UTF-8 encoded strings and will attempt to convert them to UTF-16 before passing them to Windows API functions. However, if the library fails to convert a filename string to UTF-16, it will now fallback to the equivalent Windows "ANSI" API functions which will interpret the string according to the active Windows code page.

   Support for a new environment variable, HDF5_PREFER_WINDOWS_CODE_PAGE, was added in order to instruct HDF5 to prefer interpreting filenames according to the active Windows code page rather than assuming UTF-8 encoding. If this environment variable is set to "1" or "TRUE" (case-insensitive), the active code page will be preferred. If it is unset or set to "0" or "FALSE" (case-insensitive), UTF-8 will be preferred.

### Fixed an issue with caching in the ROS3 VFD
   The ROS3 VFD uses a very simple caching mechanism that caches the first 16MiB of a file during file open and serves later reads from that cache if the offset + length falls within the cached range of bytes. Combinations of offset + length that extended exactly to the end of the cached range of bytes (for example, offset=0 and len=16777216) would end up not being served from the cache due to an incorrect range check. This has now been fixed.

### Fixed an error with `H5Fget_file_image()` with the latest file format
   When using `H5Fget_file_image()` on a file created with the latest file format (or any format newer than the earliest), the library failed to recalculate the superblock checksum after changing the access flags in the superblock, causing any subsequent attempt to open the returned file image to fail due to the checksum failing to verify. Fixed `H5Fget_file_image()` to recalculate the checksum.

   Fixed GitHub issue [#1915](https://github.com/HDFGroup/hdf5/issues/1915)

### Fixed an assertion failure in `H5S__hyper_make_spans()`

   Calling H5Sselect_hyperslab() on dataspaces with invalid extents could result in an assertion failure in debug builds of the library if the dataspace has an extent with a rank value of 0. This has been fixed by converting the assertion failure into a normal error check.

### Fixed an assertion failure in `H5S__hyper_new_span_info()`

  Calling `H5Scopy()` on hyperslab selection dataspaces with invalid extents could result in an assertion failure in debug builds of the library if the dataspace has an extent with a rank value of 0. This has been fixed by converting the assertion failure into a normal error check.

### Fixed a segfault in `H5S__get_select_hyper_blocklist()`

  When attempting to retrieve the list of hyperslab blocks selected within a dataspace, a segfault or bus error could occur when the dataspace has an extent with a rank value of 0. This would cause indexing into an array variable on the stack using a negative value. An error check was added to return failure from the function for such dataspaces.

### Fixed an error in `H5Ddebug`

   `H5Ddebug` would fail for any chunked dataset with a chunk index, due to its failure to tag the dataset before performing metadata operations. This caused `h5ls -va` to silently fail to print chunk addresses. This has been fixed.

### Fixed a bug in the `H5Oexists` and `H5Oexists_by_name` API routines that would cause those routines to return FAIL instead of FALSE when checking the existence of a non-existent object with a file ID instead of a group ID.

### Fixed a segfault in h5dump when a B-tree node level is corrupted
   `h5dump` produced a segfault on a malformed file because a B-tree node level was corrupted.

    An internal function was modified to help detect when a decoded B-tree node level has an unexpected value, and an error will be produced.

    Fixed GitHub issue [#4432](https://github.com/HDFGroup/hdf5/issues/4432)

### Fixed `H5Ovisit2` to recursively visit all objects

    `H5Ovisit2` visited only the root group and not all the nested groups.

    This behavior occurred when the fields are not `H5O_INFO_BASIC` or `H5O_INFO_ALL` because an internal function did not obtain the basic information needed by its caller. This problem is now fixed.

   Fixed GitHub issue [#4941](https://github.com/HDFGroup/hdf5/issues/4941)

### Only clear `FE_INVALID` when that symbol is present on the system

   When we initialize the floating-point types at library startup, it's possible to raise floating-point exceptions when we check which things are supported. Normally, we clear these floating-point exceptions via `feclearexcept(FE_INVALID)`, but `FE_INVALID` may not be present on all systems. Specifically, this was reported as being a problem when using Emscripten 3.1.68 to compile HDF5 1.14.5 to WebAssembly.

   We've added an #ifdef `FE_INVALID` block around the exception clearing code to correct this.

   Fixed GitHub issue [#4952](https://github.com/HDFGroup/hdf5/issues/4952)

### Fixed security issue CVE-2025-2310

   A malformed HDF5 file could have an attribute with a recorded name length of zero.This would lead to an overflow and an invalid memory access. An integrity check
   has been added to detect this case and safely stop file decoding.

## Java Library

### Renamed the Callbacks.java file to H5Callbacks.java

  The Callbacks.java file was renamed to H5Callbacks.java to match the file pattern used by doxygen. This change only affects the Java filenames and does not change the classname or the package name.

## Configuration

### Reorganized CMake HDF5 configuration options

   The CMake configuration options have been reorganized to identify the primary options that are relevant to the build. These options are now in a separate file, CMakeBuildOptions.cmake, which is included by the root CMakeLists.txt file. In addition, some options have been converted to `cmake_dependent_option()` calls, which allows the options to be hidden from the CMake GUI when they are not relevant to the build.

### Remove default setting of `CMAKE_DEBUG_POSTFIX`

   Move the default setting of `CMAKE_DEBUG_POSTFIX` to the cacheinit.cmake file usually used by testing. If `CMAKE_DEBUG_POSTFIX` is not set with a -D option then `CMAKE_DEBUG_POSTFIX` will be the default provided by CMake itself.

### The relative rpaths ($ORIGIN / @loader_path) are appended to the `CMAKE_INSTALL_RPATH`

   The RPATH settings were removed by a pull-request [#5271](https://github.com/HDFGroup/hdf5/pull/5271), but the settings are needed under certain conditions. These settings have been restored by appending the necessary paths and will not override/overwrite any existing settings.

### When using a system-installed zlib library, the shared library is expected to be found in the system library path.

   Setting the `HDF5_MODULE_MODE_ZLIB` option to OFF will force find_package to use config mode first. An installed zlib, or an alternate installed zlib library, is expected to have a correct zlib-config.cmake file for config mode. Current zlib installs usually do not have a zlib-config.cmake file, so the option is set to ON by default.

### Use pre-installed libaec compression library

   The CMake logic for finding the libaec compression library has been modified for a system-installed version of the library. Two options
 must be set:

   ```
   HDF5_ALLOW_EXTERNAL_SUPPORT:STRING=NO
   <LIB_PKG_NAME>_USE_EXTERNAL:BOOL=OFF
   ```

   where `<LIB_PKG_NAME>` is one of `ZLIB`, `ZLIBNG`, `SZIP`, `PLUGIN`.

   Note that `HDF5_ALLOW_EXTERNAL_SUPPORT:STRING=NO` disables building all plugins and external libraries in-line with the HDF5 library.

   In addition, the `<LIB_PKG_NAME>_ROOT` environment variables must be set, where `<LIB_PKG_NAME>` is one of `ZLIB`, `ZLIBNG`, `SZIP`, `libaec`, `PLUGIN`. Note that libaec is the expected name for using the libaec library in place of original szip.

   See INSTALL_CMake.txt for more detailed information.

### Changed the zlib/szip compression find message to FATAL ERROR

  The message was changed to indicate that zlib/szip compression was requested and that it was not found. If an option is requested, not finding it should always be an error.

### Removed the module search `find_package` for szip library

  There is not an szip module file to use, so the `find_package` only uses `find_package` in config mode. The choice then is to either build szip, with libaec, inline, or find a system installed szip library, built with CMake.

## Tools

### `h5repack` did not properly parse User Defined filters

   The `h5repack` tool did not properly parse user-defined filter command-line arguments when the number of elements value was 0 (zero). Also, using a colon without a preceding object was enforced to behave the same as not using a colon.

   Fixed GitHub issue [#5132](https://github.com/HDFGroup/hdf5/issues/5132)

### Changed the default value for number of cd_values in filters.

   The tools used an arbitrary value 0f 20 for the number of cd_values used in a filter. Created a new define `DEFAULT_CDELEMTS` in H5tools.h for the default value, which currently matches the library restriction of 256.

  Fixed GitHub issue [#5414](https://github.com/HDFGroup/hdf5/issues/5414)

## Performance

## Fortran API

## High-Level Library

### Fixed an issue with H5TB functions

   The H5TB functions were not correctly creating the FILL_INFO attribute for tables. This has been fixed by using the field offsets array from the call to H5TBAget_fill instead of using the compound type member offsets from the H5Tget_member_offset call for each compound type.

## Fortran High-Level APIs

## Documentation

## F90 APIs

## C++ APIs

## Testing

### Fixed an allocation in the t_bigio parallel test on 32-bit systems

   A test in t_bigio.c attempts to allocate more than 4 GiB of memory, which will overflow (and wrap) the size_t type on 32-bit systems, creating a very small allocation instead of a very large allocation. The test then segfaults when it accesses memory outside of the small buffer.

   The test has been fixed by limiting the buffer to 2 GiB on 32-bit systems.

   Fixed GitHub [#2510](https://github.com/HDFGroup/hdf5/issues/2510)

### Added skipping of a few parallel tests for OpenMPI 5.0.5

    An issue in OpenMPI 5.0.5 causes a few parallel HDF5 tests (mpiodup, props, fapl_preserve) to fail. These tests are now skipped for that release of OpenMPI. The issue has been fixed in the 5.0.6 release of OpenMPI.

# ✨ Support for new platforms and languages

# ☑️ Platforms Tested

A table of platforms tested can be seen on the [wiki](https://github.com/HDFGroup/hdf5/wiki/Platforms-Tested).
Current test results are available [here](https://my.cdash.org/index.php?project=HDF5).

# ⛔ Known Problems

- When the library detects and builds in support for the _Float16 datatype, an issue has been observed on at least one MacOS 14 system where the library fails to initialize due to not being able to detect the byte order of the _Float16 type [#4310](https://github.com/HDFGroup/hdf5/issues/4310):

     #5: H5Tinit_float.c line 308 in H5T__fix_order(): failed to detect byte order
     major: Datatype
     minor: Unable to initialize object

   If this issue is encountered, support for the _Float16 type can be disabled with a configuration option:

     `CMake: HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16=OFF`

- When HDF5 is compiled with NVHPC versions 23.5 - 23.9 (additional versions may also be applicable) and with -O2 (or higher) and -DNDEBUG, test failures occur in the following tests:

      H5PLUGIN-filter_plugin <br>
      H5TEST-flush2<br>
      H5TEST-testhdf5-base<br>
      MPI_TEST_t_filters_parallel<br>

  Sporadic failures (even with lower -O levels):<br>

      Java JUnit-TestH5Pfapl<br>
      Java JUnit-TestH5D<br>

  Also, NVHPC will fail to compile the test/tselect.c test file with a compiler error of 'use of undefined value' when the optimization level is -O2 or higher.

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
