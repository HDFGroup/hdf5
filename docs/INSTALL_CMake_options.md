# HDF5 CMake build options

The tables below document the CMake options that can be set to control how HDF5 is built. Options are typically set by passing them in the form of `-D<OPTION>=<VALUE>` when configuring HDF5. For example, `-DHDF5_BUILD_FORTRAN=ON` will enable building of the HDF5 Fortran wrappers. See [the CMake documentation](https://cmake.org/cmake/help/latest/command/set.html#set-cache-entry) for a short description of option types and their associated values.

Options settings for typical HDF5 configurations can be found in the [cacheinit.cmake](../config/cmake/cacheinit.cmake) and [CMakePresets.json](../CMakePresets.json) files. 

---

## Table of Contents

* [CMake-specific options](#cmake-specific-options)
* [General options](#general-options)
  * [Installation options](#installation-options)
  * [Packaging options](#packaging-options)
  * [Compiler options](#compiler-options)
  * [MinGW-specific options](#mingw-specific-options)
* [Programming language wrappers options](#programming-language-wrappers-options)
  * [Fortran options](#pl_fortran)
  * [Java options](#pl_java)
  * [C++ options](#pl_cxx)
  * [High-level library options](#hl_lib)
* [Parallel HDF5 options](#parallel-hdf5-options)
* [Virtual File Driver options](#virtual-file-driver-options)
* [VOL connector options](#vol-connector-options)
* [Data filters options](#data-filters-options)
  * [General options](#general_filter_options)
  * [ZLib(-ng) options](#zlib-ng-options)
  * [Libaec (szip) options](#libaec-szip-options)
  * [HDF5 filter plugins options](#hdf5-filter-plugins-options)
* [HDF5 Examples options](#hdf5-examples-options)
  * [Filter plugin examples options](#filter-plugin-examples-options)
* [HDF5 testing options](#hdf5-testing-options)
* [Compiler wrapper options](#compiler-wrapper-options)
* [Debugging options](#debugging-options)
* [Sanitizer, code coverage and formatting options](#sanitizer-code-coverage-and-formatting-options)
* [Deprecated options](#deprecated-options)
* [Unsupported option combinations](#unsupported-option-combinations)

---

## CMake-specific options

These are some common options that come from CMake itself and are not specific to HDF5.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `CMAKE_INSTALL_PREFIX` | `STRING` | Varies by platform | HDF5 installation directory prefix. See [CMAKE_INSTALL_PREFIX](https://cmake.org/cmake/help/latest/variable/CMAKE_INSTALL_PREFIX.html). |
| `CMAKE_BUILD_TYPE` | `STRING` | `Release` | HDF5 build type. See [CMAKE_BUILD_TYPE](https://cmake.org/cmake/help/latest/variable/CMAKE_BUILD_TYPE.html). Valid values are `Release`, `Debug`, `RelWithDebInfo`, `MinSizeRel` and `Developer`. |

## General options

These options concern the general build process of the main HDF5 libraries, utilities, documentation, etc.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `BUILD_SHARED_LIBS` | `BOOL` | `ON` | If `ON`, builds shared HDF5 libraries. |
| `BUILD_STATIC_LIBS` | `BOOL` | `ON` | If `ON`, builds static HDF5 libraries. |
| `BUILD_STATIC_EXECS` | `BOOL` | `OFF` | If `ON`, builds statically-linked executables. **NOTE:** The `BUILD_STATIC_EXECS` option is only valid on some UNIX operating systems. It adds the `-static` flag to `CMAKE_EXE_LINKER_FLAGS`. This flag is not available on Windows and some modern Linux systems will ignore the flag. |
| `HDF5_DEFAULT_API_VERSION` | `STRING` | `v200` | Specifies the default HDF5 API version to use when compiling HDF5 libraries. Valid values are `v200` (2.x API), `v114` (1.14.x API), `v112` (1.12.x API), `v110` (1.10.x API), `v18` (1.8.x API) and `v16` (1.6.x API). See [API Compatibility Macros](https://support.hdfgroup.org/documentation/hdf5/latest/api-compat-macros.html#title5) for more information on this option. |
| `HDF5_ALLOW_UNSUPPORTED` | `BOOL` | `OFF` | If `ON`, allows configuring and building HDF5 with unsupported combinations of features. Otherwise, causes a configuration error if an unsupported combination is enabled. See [Unsupported option combinations](#unsupported_combos) for a list of unsupported combinations. |
| `HDF5_ENABLE_CONCURRENCY` | `BOOL` | `OFF` | If `ON`, enables building of a multi-thread concurrent HDF5 library. Requires C11 threads, Win32 threads or Pthreads. Requires shared HDF5 libraries on Windows. **NOTE:** Currently non-functional and experimental. |
| `HDF5_ENABLE_THREADSAFE` | `BOOL` | `OFF` | If `ON`, enables building of a thread-safe HDF5 library. Requires C11 threads, Win32 threads or Pthreads. Requires shared HDF5 libraries on Windows. |
| `HDF5_ENABLE_NONSTANDARD_FEATURES` | `BOOL` | `ON` | If `ON`, enables non-standard programming language features. If `OFF`, disables all non-standard programming language features. Each feature has its own separate option. |
| `HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16` | `BOOL` | `ON` (if `_Float16` type is supported) | If `ON`, enables building of support for the `_Float16` 16-bit floating-point datatype. |
| `HDF5_BUILD_TOOLS` | `BOOL` | `ON` | If `ON`, enables building of the HDF5 tool programs (`h5dump`, `h5ls`, etc.) |
| `HDF5_BUILD_STATIC_TOOLS` | `BOOL` | `OFF` | If `ON`, HDF5 tool programs are linked against static HDF5 libraries when built. Otherwise, HDF5 tool programs are linked against shared HDF5 libraries when built. Has no effect if `HDF5_BUILD_TOOLS` is `OFF`. If `ON` and `BUILD_STATIC_LIBS` is `OFF`, causes a configuration error. |
| `HDF5_BUILD_UTILS` | `BOOL` | `ON` | If `ON`, enables building of additional HDF5 utility programs, scripts, etc. |
| `HDF5_BUILD_DOC` | `BOOL` | `OFF` | If `ON`, enables building of HDF5 documentation. Requires doxygen to be found by CMake in order to build. |
| `HDF5_ENABLE_DEPRECATED_SYMBOLS` | `BOOL` | `ON` | If `ON`, deprecated public HDF5 API symbols are built into its libraries. Otherwise, these symbols will not be available. |
| `HDF5_USE_FILE_LOCKING` | `BOOL` | `ON` | If `ON`, uses file locking in Virtual File Drivers by default, if supported. More information about file locking in HDF5 can be found at [File Locking in HDF5](https://support.hdfgroup.org/documentation/hdf5/latest/_file_lock.html). |
| `HDF5_IGNORE_DISABLED_FILE_LOCKS` | `BOOL` | `ON` | If `ON`, ignores file locks when file locking is disabled on a file system. More information about file locking in HDF5 can be found at [File Locking in HDF5](https://support.hdfgroup.org/documentation/hdf5/latest/_file_lock.html). |
| `HDF5_STRICT_FORMAT_CHECKS` | `BOOL` | `OFF` | If `ON`, enables stricter checking of adherence to the HDF5 file format when reading or writing files. This option can cause errors for some files that were written with earlier, buggy versions of the HDF5 library. |
| `HDF5_WANT_DATA_ACCURACY` | `BOOL` | `ON` | If `ON`, instructs the HDF5 library that data accuracy is more important than optimized performance when performing datatype conversions. Otherwise, the library may prefer to use conversion functions that optimize conversion performance in exchange for data accuracy. |
| `HDF5_WANT_DCONV_EXCEPTION` | `BOOL` | `ON` | If `ON`, exceptions during datatype conversions will cause application-provided callbacks to be called (if set) and the specific exceptions to be returned. Setting this option to `OFF` may improve performance of datatype conversions, with the tradeoff of ignoring datatype conversion exceptions which occur. |
| `HDF5_ENABLE_PREADWRITE` | `BOOL` | `ON` | If `ON`, use `pread(2)` / `pwrite(2)` in Virtual File Drivers if the functions are available. Otherwise, `read(2)` / `write(2)` are used instead. |
| `HDF5_ENABLE_MAP_API` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 Map (`H5M`) API for storing and interacting with key-value pair data. |
| `HDF5_EXTERNAL_LIB_PREFIX` | `STRING` | `""` (empty string) | Specifies a prefix string to use for customizing HDF5 library names. |
| `HDF5_LIB_INFIX` | `STRING` | `""` (empty string) | Specifies an infix string to use for customizing HDF5 library names. This infix string is added to all library names after "hdf5". |
| `HDF5_EXTERNAL_LIB_SUFFIX` | `STRING` | `""` (empty string) | Specifies a suffix string to use for customizing HDF5 library names. |
| `HDF5_ENABLE_INSTRUMENT` | `BOOL` | `OFF` | If `ON`, enables instrumentation of some parallel HDF5 functions. Depends on `CMAKE_BUILD_TYPE` being `Debug` or `Developer`. |
| `HDF5_ENABLE_DOXY_WARNINGS` | `BOOL` | `OFF` | If `ON`, causes a build failure if doxygen parsing has warnings. Has no effect if  `HDF5_BUILD_DOC` is `OFF`. |
| `HDF5_USE_FOLDERS` | `BOOL` | `ON` | If `ON`, enables folder grouping of HDF5 build projects in IDEs. See [USE_FOLDERS](https://cmake.org/cmake/help/latest/prop_gbl/USE_FOLDERS.html) for more information. |
| `HDF5_GENERATE_HEADERS` | `BOOL` | `OFF` | If `ON`, enables (re-)generation of some HDF5 source files. Intended for HDF5 developers only. Requires perl. |

### Installation options

These options control how HDF5 gets installed. Options dealing with paths are generally relative to `CMAKE_INSTALL_PREFIX`.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_USE_GNU_DIRS` | `BOOL` | `OFF` | If `ON`, uses the GNU Coding Standard CMake install directory variables when setting up for installing the HDF5 library. See [GNUInstallDirs](https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html) for more information. |
| `HDF5_INSTALL_BIN_DIR` | `STRING` | `bin` | Specifies the directory to install executables in. |
| `HDF5_INSTALL_LIB_DIR` | `STRING` | `lib` | Specifies the directory to install libraries in. |
| `HDF5_INSTALL_JNI_LIB_DIR` | `STRING` | `lib` | Specifies the directory to install Java JNI libraries in. |
| `HDF5_INSTALL_INCLUDE_DIR` | `STRING` | `include` | Specifies the directory to install header files in. |
| `HDF5_INSTALL_MODULE_DIR` | `STRING` | `mod` (`HDF5_USE_GNU_DIRS=OFF`) <br /> `HDF5_INSTALL_INCLUDE_DIR/mod` (`HDF5_USE_GNU_DIRS=ON`) | Specifies the directory to install Fortran .mod files in. |
| `HDF5_INSTALL_CMAKE_DIR` | `STRING` | `cmake` (`HDF5_USE_GNU_DIRS=OFF`) <br /> `HDF5_INSTALL_LIB_DIR/cmake` (`HDF5_USE_GNU_DIRS=ON`) | Specifies the directory to install CMake files in. |
| `HDF5_INSTALL_DATA_DIR` | `STRING` | `.` (for `MSVC` and `HDF5_USE_GNU_DIRS=OFF`) <br /> `share` (`HDF5_USE_GNU_DIRS=ON`) | Specifies the directory to install miscellaneous data files in. |
| `HDF5_INSTALL_DOC_DIR` | `STRING` | `HDF5_INSTALL_DATA_DIR` (`HDF5_USE_GNU_DIRS=OFF`) <br /> `HDF5_INSTALL_DATA_DIR/doc/hdf5` (`HDF5_USE_GNU_DIRS=ON`) | Specifies the directory to install documentation files in. |
| `HDF5_BUILD_WITH_INSTALL_NAME` | `BOOL` | `OFF` | **MacOS only** If `ON`, builds shared library CMake targets with the "install_name" field set to the installation path. See the related CMake property [INSTALL_NAME_DIR](https://cmake.org/cmake/help/latest/prop_tgt/INSTALL_NAME_DIR.html#prop_tgt:INSTALL_NAME_DIR). |
| `HDF5_DISABLE_PDB_FILES` | `BOOL` | `OFF` | **Windows only** If `ON`, do not install PDB files. |

> **NOTE:** The `HDF5_USE_GNU_DIRS` option is usually recommended for Linux platforms, but may be useful on other platforms. See the CMake documentation for more details.

### Packaging options

These options control how platform-specific binary installers and source packages of HDF5 get built using CMake's [CPack](https://cmake.org/cmake/help/latest/module/CPack.html) functionality.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `CPACK_GENERATOR` | `STRING` | Varies by platform | A semicolon-separated list of packages to generate with CPack. See [cpack-generators](https://cmake.org/cmake/help/latest/manual/cpack-generators.7.html) and the CPack documentation for more information on the package generators that may be available for your platform and how to use them. |
| `HDF5_NO_PACKAGES` | `BOOL` | `OFF` | If `ON`, disables CPack support and the ability to create HDF5 packages. |
| `HDF5_PACKAGE_EXTLIBS` | `BOOL` | `OFF` | If `ON`, HDF5 packages created with CPack will include any external libraries that were built alongside HDF5, such as zlib. **NOTE:** Be aware that this could overwrite system libraries if these packages are installed to a system-wide location. |
| `HDF5_PACK_EXAMPLES` | `BOOL` | `OFF` | If `ON`, HDF5 packages created with CPack will include the HDF5 example programs. |
| `HDF5_BUILD_FRAMEWORKS` | `BOOL` | `OFF` | If `ON`, builds HDF5 as a framework bundle when built on MacOS. |
| `HDF5_PACK_MACOSX_DMG` | `BOOL` | `ON` | If `ON`, CPack will create a .dmg image package for HDF5 when built on MacOS. |
| `HDF5_PACK_MACOSX_FRAMEWORK` | `BOOL` | `OFF` | If `ON`, CPack will create a framework bundle for HDF5 when built on MacOS. |
| `HDF_PACKAGE_NAMESPACE` | `STRING` | `hdf5::` | Specifies a string to use for namespacing CMake targets created by HDF5. |
| `HDF_PACKAGE_EXT` | `STRING` | `""` (empty string) | Specifies a suffix to add to the names of several files included in HDF5 packages created. |

### Compiler options

These options are generally passed through to the compiler to influence most or all of the build process.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_ALL_WARNINGS` | `BOOL` | `ON` | If `ON`, enables "all" compiler warnings, similar to using `-Wall` (though not necessarily equivalent). |
| `HDF5_ENABLE_DEV_WARNINGS` | `BOOL` | `OFF` | If `ON`, enables some additional compiler warnings which are mostly targeted toward HDF5 developers (e.g., flags such as `-Wsuggest-attribute` and `-Wunused-macros`). |
| `HDF5_SHOW_ALL_WARNINGS` | `BOOL` | `OFF` | If `ON`, shows all compiler warnings instead of suppressing warnings internally in specific locations in the source code. |
| `HDF5_DISABLE_COMPILER_WARNINGS` | `BOOL` | `OFF` | If `ON`, disables most or all compiler warnings. |
| `HDF5_ENABLE_WARNINGS_AS_ERRORS` | `BOOL` | `OFF` | If `ON`, causes some compiler warnings to be treated as errors. |
| `HDF5_ENABLE_BUILD_DIAGS` | `BOOL` | `OFF` | If `ON`, enables the use of color and URLs in compiler diagnostic messages. |
| `HDF5_ENABLE_ASSERTS` | `STRING` | `OFF` | If `YES`, enables assertions when building HDF5, regardless of the build type (adds `-UNDEBUG` to compilation flags). If `NO`, adds `-DNDEBUG` to compilation flags. Valid values are `YES`, `NO` and `OFF`. |
| `HDF5_ENABLE_SYMBOLS` | `STRING` | `OFF` | If `YES`, adds compilation flags to enable debugging symbols, independent of the build type and optimization level. If `NO`, adds compilation flags to strip debugging symbols. Valid values are `YES`, `NO` and `OFF`. |
| `HDF5_ENABLE_PROFILING` | `BOOL` | `OFF` | If `ON`, adds compilation flags for profiling, independent of the build type. |
| `HDF5_ENABLE_OPTIMIZATION` | `BOOL` | `OFF` | If `ON`, adds compilation flags for optimization, independent of the build type. |

### MinGW-specific options

These are options that are specific to building HDF5 with MinGW.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_MINGW_STATIC_GCC_LIBS` | `BOOL` | `OFF` | If `ON`, statically link `libgcc` / `libstdc++` when using a MinGW toolchain. |
| `HDF5_MSVC_NAMING_CONVENTION` | `BOOL` | `OFF` | If `ON`, use MSVC naming convention for shared HDF5 libraries when using a MinGW toolchain. |

## Programming language wrappers options

These are options which are specific to HDF5's wrappers for programming languages.

### Fortran options
<a name="pl_fortran"></a>

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_BUILD_FORTRAN` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 Fortran wrapper interface. |
| `HDF5_INSTALL_MOD_FORTRAN` | `STRING` | `SHARED` if HDF5 shared libraries are built; `STATIC` otherwise | If `SHARED` or `STATIC`, installs Fortran .mod files to a subdirectory (named `shared` or `static`) of the installation's `include` directory. Valid values are `SHARED`, `STATIC` and `NO`. |

### Java options
<a name="pl_java"></a>

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_BUILD_JAVA` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 Java wrapper interface. |
| `HDF5_ENABLE_JNI` | `BOOL` | `ON` if `HDF5_BUILD_JAVA` is `ON` | If `ON`, enables building of the HDF5 Java JNI (Java Native Interface) bindings. Otherwise, enables building of the HDF5 Java FFM (Foreign Function and Memory) bindings, which requires the `jextract` tool to be found by CMake. |
| `HDF5_JAVA_PACK_JRE` | `BOOL` | `OFF` | If `ON`, packages a Java JRE with an HDF5 installation. |
| `HDF5_ENABLE_MAVEN_DEPLOY` | `BOOL` | `OFF` | If `ON`, enables Maven repository deployment support. |
| `HDF5_MAVEN_SNAPSHOT` | `BOOL` | `OFF` | If `ON`, adds "-SNAPSHOT" to version portion of names of Maven artifacts. |

### C++ options
<a name="pl_cxx"></a>

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_BUILD_CPP_LIB` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 C++ wrapper interface. |

### High-level C library options
<a name="hl_lib"></a>

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_BUILD_HL_LIB` | `BOOL` | `ON` | If `ON`, enables building of the HDF5 high-level C wrapper interface. |
| `HDF5_DIMENSION_SCALES_NEW_REF` | `BOOL` | `OFF` | If `ON`, the high-level C dimension scale API (H5DS) will use a newer format for reference objects. The dimension scales created in data files produced this way cannot be read by HDF5 library versions below HDF5 1.12.0. |

## Parallel HDF5 options

These are options which are specific to parallel HDF5.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_PARALLEL` | `BOOL` | `OFF` | If `ON`, enables building of parallel HDF5. Requires an MPI implementation of at least the [MPI 3.0 standard](https://www.mpi-forum.org/docs/mpi-3.0/mpi30-report.pdf). |
| `HDF5_ENABLE_SUBFILING_VFD` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 [Subfiling VFD](https://github.com/HDFGroup/hdf5doc/blob/master/RFCs/HDF5_Library/VFD_Subfiling/user_guide/HDF5_Subfiling_VFD_User_s_Guide.pdf). Requires C11 threads, Win32 threads or Pthreads. |
| `HDF5_BUILD_PARALLEL_TOOLS` | `BOOL` | `OFF` | If `ON`, enables building of parallel HDF5 tools. Currently experimental. |
| `MPI_C_COMPILER` | `FILEPATH` | Varies | Path to the program to be used for compiling MPI C source code. Usually auto-detected by CMake. |
| `MPIEXEC_EXECUTABLE` | `FILEPATH` | Varies | Path to the program to be used for running MPI programs. Usually auto-detected by CMake. |
| `MPIEXEC_MAX_NUMPROCS` | `STRING` | Varies | Maximum number of MPI processes to use when running MPI programs. Usually auto-detected by CMake. |
| `MPIEXEC_NUMPROC_FLAG` | `STRING` | Varies | Command-line flag to pass to `MPIEXEC_EXECUTABLE` for specifying the maximum number of MPI processes. Usually auto-detected by CMake. |
| `MPIEXEC_PREFLAGS` | `STRING` | `""` (empty string) | Additional user-defined flags to pass to `MPIEXEC_EXECUTABLE` when running MPI programs. Any flags specified for this option will come directly before the name of the executable being run. |
| `MPIEXEC_POSTFLAGS` | `STRING` | `""` (empty string) | Additional user-defined flags for `MPIEXEC_EXECUTABLE` to pass as arguments for the MPI programs that it runs. Any flags specified for this option will come directly after the name of the executable being run. |

## Virtual File Driver options

These options are specific to HDF5 [Virtual File Drivers](https://support.hdfgroup.org/documentation/hdf5/latest/_v_f_l_t_n.html).

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_DIRECT_VFD` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 [Direct I/O VFD](https://support.hdfgroup.org/documentation/hdf5/latest/_h5_f__u_g.html#subsubsec_file_alternate_drivers_direct). Requires POSIX direct I/O support and `posix_memalign()`. |
| `HDF5_ENABLE_MIRROR_VFD` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 Mirror VFD.  |
| `HDF5_ENABLE_HDFS` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 HDFS VFD. |
| `HDF5_ENABLE_ROS3_VFD` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 [ROS3 (read-only S3) VFD](https://support.hdfgroup.org/documentation/hdf5/latest/_h5_f__u_g.html#subsubsec_file_alternate_drivers_ros3). Requires the [aws-c-s3](https://github.com/awslabs/aws-c-s3) library. |
| `HDF5_ENABLE_ROS3_VFD_DOCKER_PROXY` | `BOOL` | `OFF` | If `ON`, enables testing of the HDF5 ROS3 VFD using `docker` and `aws-cli`. |

## VOL connector options

These are options which control how HDF5 VOL connectors get built when building HDF5. For more information about how to use these options, see [Building and testing HDF5 VOL connectors with CMake FetchContent](https://support.hdfgroup.org/documentation/hdf5/latest/_c_make_vols.html).

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_VOL_ALLOW_EXTERNAL` | `STRING` | `NO` | If `GIT` or `LOCAL_DIR`, enables building of HDF5 VOL connectors from external sources while building HDF5. Valid values are `GIT`, `LOCAL_DIR` and `NO`. |
| `HDF5_VOL_URL(0-9)` | `STRING` | `""` (empty string) | If `HDF5_VOL_ALLOW_EXTERNAL` is `GIT`, these placeholder variables (`HDF5_VOL_URL0`, `HDF5_VOL_URL1`, ...) are defined to allow specifying git URLs to retrieve source code for VOL connectors from when building HDF5. |
| `HDF5_VOL_PATH(0-9)` | `STRING` | `""` (empty string) | If `HDF5_VOL_ALLOW_EXTERNAL` is `LOCAL_DIR`, these placeholder variables (`HDF5_VOL_PATH0`, `HDF5_VOL_PATH1`, ...) are defined to allow specifying file paths to retrieve source code for VOL connectors from when building HDF5. |
| `HDF5_VOL_${vol_name_upper}_BRANCH` | `STRING` | `main` | If `HDF5_VOL_ALLOW_EXTERNAL` is `GIT`, then for each VOL connector specified by a git URL (`HDF5_VOL_URL0`, `HDF5_VOL_URL1`, ...) a CMake variable will be created to allow specifying a git tag, commit hash or branch name to use when retrieving that VOL connector from git. The process for how `vol_name_upper` is determined is documented more in the link above. |
| `HDF5_VOL_${vol_name_upper}_NAME` | `STRING` | `""` (empty string) | For each VOL connector specified by a git URL (`HDF5_VOL_URL0`, `HDF5_VOL_URL1`, ...) or file path (`HDF5_VOL_PATH0`, `HDF5_VOL_PATH1`, ...), a CMake variable will be created to allow specifying the VOL connector name to use when running HDF5 tests. The process for how `vol_name_upper` is determined is documented more in the link above. |
| `HDF5_VOL_${vol_name_upper}_CMAKE_PACKAGE_NAME` | `STRING` | `${vol_name_lower}` | For each VOL connector specified by a git URL (`HDF5_VOL_URL0`, `HDF5_VOL_URL1`, ...) or file path (`HDF5_VOL_PATH0`, `HDF5_VOL_PATH1`, ...), a CMake variable will be created to allow specifying the package name to use when finding a VOL connector with CMake's [find_package()](https://cmake.org/cmake/help/latest/command/find_package.html). The process for how `vol_name_upper` and `vol_name_lower` are determined is documented more in the link above. |
| `HDF5_VOL_${vol_name_upper}_TEST_PARALLEL` | `BOOL` | `OFF` | If `ON`, enables testing of HDF5's parallel tests with the VOL connector specified by `vol_name_upper`. The process for how `vol_name_upper` is determined is documented more in the link above. |

## Data filters options

These are options specific to HDF5 data filters. The options are given a brief overview here but are covered in more detail in [INSTALL_Filters.md](./INSTALL_Filters.md).

### General options
<a name="general_filter_options"></a>

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ALLOW_EXTERNAL_SUPPORT` | `STRING` | `NO` | If `GIT` or `TGZ`, enables building of data filter libraries from external sources while building HDF5. Valid values are `GIT`, `TGZ` and `NO`. |
| `H5_DEFAULT_PLUGINDIR` | `STRING` | `${CMAKE_INSTALL_PREFIX}/lib/plugin:/usr/local/hdf5/lib/plugin` (Unix) <br /> `${CMAKE_INSTALL_PREFIX}\lib\plugin;%ALLUSERSPROFILE%\hdf5\lib\plugin` (Windows) | Specifies the default location(s) that HDF5 will search in for data filter plugins. On Unix-like systems, paths should be separated with `:`. On Windows systems, paths should be separated with `;`. |

### ZLib(-ng) options

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_ZLIB_SUPPORT` | `BOOL` | `OFF` | If `ON`, enables building of support for zlib data compression. If zlib libraries are not found, causes a configuration error. |
| `HDF5_USE_ZLIB_NG` | `BOOL` | `OFF` | If `ON`, enables building of support for zlib data compression using zlib-ng libraries instead of zlib libraries. If zlib-ng libraries are not found, causes a configuration error. Has no effect if `HDF5_ENABLE_ZLIB_SUPPORT` is `OFF`. |
| `HDF5_USE_ZLIB_STATIC` | `BOOL` | `OFF` | If `ON`, prefer locating and using static zlib(-ng) libraries (if available) instead of shared libraries. |
| `HDF5_MODULE_MODE_ZLIB` | `BOOL` | `ON` | If `ON`, locates zlib using CMake's [find_package()](https://cmake.org/cmake/help/latest/command/find_package.html) Module mode search, otherwise uses a Config mode search. In most cases this option should be left to the default value, but may need to be modified for specific zlib installations. |
| `ZLIB_USE_EXTERNAL` | `BOOL` | `OFF` | If `ON`, specifies that zlib libraries should be retrieved from an external source and should be built while building HDF5. |
| `ZLIB_USE_LOCALCONTENT` | `BOOL` | `OFF` | If `ON`, specifies that zlib libraries should be found from a file on the filesystem and should be built while building HDF5. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIB_VERSION` | `STRING` | Check [CacheURLs.cmake](../config/CacheURLs.cmake) for the current version | Specifies the version of zlib library sources to retrieve when building zlib from source. |
| `ZLIBNG_VERSION` | `STRING` | Check [CacheURLs.cmake](../config/CacheURLs.cmake) for the current version | Specifies the version of zlib-ng library sources to retrieve when building zlib-ng from source. |
| `ZLIB_GIT_URL` | `STRING` | `https://github.com/madler/zlib.git` | Specifies the git URL to retrieve zlib library sources from when building zlib from source. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIBNG_GIT_URL` | `STRING` | `https://github.com/zlib-ng/zlib-ng.git` | Specifies the git URL to retrieve zlib-ng library sources from when building zlib-ng from source. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIB_GIT_TAG` | `STRING` | `v<zlib_version>` | Specifies the git tag, commit hash or branch name to use for retrieving zlib library sources when building zlib from source. `<zlib_version>` comes from `ZLIB_VERSION` above. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`.  |
| `ZLIBNG_GIT_TAG` | `STRING` | `<zlibng_version>` | Specifies the git tag, commit hash or branch name to use for retrieving zlib-ng library sources when building zlib-ng from source. `<zlibng_version>` comes from `ZLIBNG_VERSION` above. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIB_TGZ_NAME` | `STRING` | `zlib-<zlib_version>.tar.gz` | Specifies the name of a compressed file of zlib library sources when building zlib from source. `<zlib_version>` comes from `ZLIB_VERSION` above. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIBNG_TGZ_NAME` | `STRING` | `<zlibng_version>.tar.gz` | Specifies the name of a compressed file of zlib-ng library sources when building zlib-ng from source. `<zlibng_version>` comes from `ZLIBNG_VERSION` above. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIB_TGZ_ORIGPATH` | `STRING` | `https://github.com/madler/zlib/releases/download/v<zlib_version>` | Specifies the URL to retrieve a compressed file of zlib library sources from when building zlib from source. `<zlib_version>` comes from `ZLIB_VERSION` above. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIBNG_TGZ_ORIGPATH` | `STRING` | `https://github.com/zlib-ng/zlib-ng/archive/refs/tags` | Specifies the URL to retrieve a compressed file of zlib-ng library sources from when building zlib-ng from source. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIB_PACKAGE_NAME` | `STRING` | `zlib` | Specifies the package name to use when locating zlib libraries with CMake's [find_package()](https://cmake.org/cmake/help/latest/command/find_package.html). Usually should be left to the default value but may need to be modified in rare circumstances. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |
| `ZLIBNG_PACKAGE_NAME` | `STRING` | `ZLIBNG` | Specifies the package name to use when locating zlib-ng libraries with CMake's [find_package()](https://cmake.org/cmake/help/latest/command/find_package.html). Usually should be left to the default value but may need to be modified in rare circumstances. Has no effect if `ZLIB_USE_EXTERNAL` is `OFF`. |

### Libaec (szip) options

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_SZIP_SUPPORT` | `BOOL` | `OFF` | If `ON`, enables building of support for libaec data compression. If libaec libraries are not found, causes a configuration error. |
| `HDF5_USE_LIBAEC_STATIC` | `BOOL` | `OFF` | If `ON`, prefer locating and using static libaec libraries (if available) instead of shared libraries. |
| `HDF5_ENABLE_SZIP_ENCODING` | `BOOL` | `ON` | If `ON`, specifies that encoding should be enabled for the HDF5 szip filter. |
| `SZIP_USE_EXTERNAL` | `BOOL` | `OFF` | If `ON`, specifies that libaec libraries should be retrieved from an external source and should be built while building HDF5. |
| `LIBAEC_USE_LOCALCONTENT` | `BOOL` | `OFF` | If `ON`, specifies that libaec libraries should be found from a file on the filesystem and should be built while building HDF5. Has no effect if `SZIP_USE_EXTERNAL` is `OFF`. |
| `LIBAEC_VERSION` | `STRING` | Check [CacheURLs.cmake](../config/CacheURLs.cmake) for the current version | Specifies the version of libaec library sources to retrieve when building libaec from source. |
| `LIBAEC_GIT_URL` | `STRING` | `https://github.com/MathisRosenhauer/libaec.git` | Specifies the git URL to retrieve libaec library sources from when building libaec from source. Has no effect if `SZIP_USE_EXTERNAL` is `OFF`. |
| `LIBAEC_GIT_TAG` | `STRING` | `v<libaec_version>` | Specifies the git tag, commit hash or branch name to use for retrieving libaec library sources when building libaec from source. Has no effect if `SZIP_USE_EXTERNAL` is `OFF`. |
| `LIBAEC_TGZ_NAME` | `STRING` | `libaec-<libaec_version>.tar.gz` | Specifies the name of a compressed file of libaec library sources when building libaec from source. Has no effect if `SZIP_USE_EXTERNAL` is `OFF`. |
| `LIBAEC_TGZ_ORIGPATH` | `STRING` | `https://github.com/MathisRosenhauer/libaec/releases/download/v<libaec_version>` | Specifies the URL to retrieve a compressed file of libaec library sources from when building libaec from source. Has no effect if `SZIP_USE_EXTERNAL` is `OFF`. |
| `LIBAEC_PACKAGE_NAME` | `STRING` | `libaec` | Specifies the package name to use when locating libaec libraries with CMake's [find_package()](https://cmake.org/cmake/help/latest/command/find_package.html). Usually should be left to the default value but may need to be modified in rare circumstances. Has no effect if `SZIP_USE_EXTERNAL` is `OFF`. |

### HDF5 filter plugins options

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_PLUGIN_SUPPORT` | `BOOL` | `OFF` | If `ON`, enables building of several HDF5 data filter plugin libraries from the [HDF5 filter plugins](https://github.com/HDFGroup/hdf5_plugins) repository. If HDF5 data filter plugin libraries are not found, causes a configuration error. |
| `PLUGIN_USE_EXTERNAL` | `BOOL` | `OFF` | If `ON`, specifies that HDF5 filter plugin libraries should be retrieved from an external source and should be built while building HDF5. |
| `PLUGIN_USE_LOCALCONTENT` | `BOOL` | `OFF` | If `ON`, specifies that HDF5 filter plugin libraries should be found from a file on the filesystem and should be built while building HDF5. Has no effect if `PLUGIN_USE_EXTERNAL` is `OFF`. |
| `HDF5_FILTER_PLUGINS_GIT_URL` | `STRING` | [hdf5_plugins](https://github.com/HDFGroup/hdf5_plugins.git) | Specifies the git URL to retrieve HDF5 filter plugin library sources from when building the HDF5 filter plugins from source. Has no effect if `PLUGIN_USE_EXTERNAL` is `OFF`. |
| `HDF5_FILTER_PLUGINS_GIT_TAG` | `STRING` | `master` | Specifies the git tag, commit hash or branch name to use for retrieving HDF5 filter plugin library sources when building the HDF5 filter plugins from source. Has no effect if `PLUGIN_USE_EXTERNAL` is `OFF`. |
| `HDF5_FILTER_PLUGINS_TGZ_NAME` | `STRING` | `hdf5_plugins-master.tar.gz` | Specifies the name of a compressed file of HDF5 filter plugin library sources when building the HDF5 filter plugins from source. Has no effect if `PLUGIN_USE_EXTERNAL` is `OFF`. |
| `HDF5_FILTER_PLUGINS_TGZ_ORIGPATH` | `STRING` | `https://github.com/HDFGroup/hdf5_plugins/releases/download/snapshot` | Specifies the URL to retrieve a compressed file of HDF5 filter plugin library sources from when building the HDF5 filter plugins from source. Has no effect if `PLUGIN_USE_EXTERNAL` is `OFF`. |
| `HDF5_FILTER_PLUGINS_PACKAGE_NAME` | `STRING` | `h5pl` | Specifies the package name to use when locating HDF5 filter plugin libraries with CMake's [find_package()](https://cmake.org/cmake/help/latest/command/find_package.html). Usually should be left to the default value but may need to be modified in rare circumstances. Has no effect if `PLUGIN_USE_EXTERNAL` is `OFF`. |

## HDF5 Examples options

These are options which can be set for controlling how the HDF5 example programs are built.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_BUILD_EXAMPLES` | `BOOL` | `ON` | If `ON`, enables building of the HDF5 library (C) example programs. |
| `USE_SHARED_LIBS` | `BOOL` | `ON` | If `ON`, build the HDF5 library example programs against shared HDF5 libraries. Otherwise, use static HDF5 libraries. |
| `H5EXAMPLE_BUILD_FORTRAN` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 library Fortran example programs. |
| `H5EXAMPLE_BUILD_JAVA` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 library Java example programs. |
| `H5EXAMPLE_BUILD_CXX` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 library C++ example programs. |
| `H5EXAMPLE_BUILD_HL` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 library high-level C example programs. |
| `H5EXAMPLE_ENABLE_PARALLEL` | `BOOL` | `OFF` | If `ON`, enables building of the parallel HDF5 library example programs. |
| `H5EXAMPLE_BUILD_PYTHON` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 library Python ([h5py](https://docs.h5py.org/en/stable/)) example programs. Python3 support must be available. |
| `H5EXAMPLE_BUILD_FILTERS` | `BOOL` | `OFF` | If `ON`, enables building of the HDF5 library filter plugins example programs. `USE_SHARED_LIBS` must be `ON` and shared HDF5 libraries must be available. |
| `H5EXAMPLE_BUILD_TESTING` | `BOOL` | `OFF` | If `ON`, enables testing of the HDF5 library example programs. |
| `H5EXAMPLE_USE_200_API` | `BOOL` | `OFF` | If `ON`, compile the HDF5 library examples programs using the HDF5 2.0.0 API. |
| `H5EXAMPLE_USE_114_API` | `BOOL` | `OFF` | If `ON`, compile the HDF5 library examples programs using the HDF5 1.14 API. |
| `H5EXAMPLE_USE_112_API` | `BOOL` | `OFF` | If `ON`, compile the HDF5 library examples programs using the HDF5 1.12 API. |
| `H5EXAMPLE_USE_110_API` | `BOOL` | `OFF` | If `ON`, compile the HDF5 library examples programs using the HDF5 1.10 API. |
| `H5EXAMPLE_USE_18_API` | `BOOL` | `OFF` | If `ON`, compile the HDF5 library examples programs using the HDF5 1.8 API. |
| `H5EXAMPLE_USE_16_API` | `BOOL` | `OFF` | If `ON`, compile the HDF5 library examples programs using the HDF5 1.6 API. |
| `H5EXAMPLE_USE_GNU_DIRS` | `BOOL` | `OFF` | If `ON`, uses the GNU Coding Standard CMake install directory variables when setting up for installing the HDF5 library example programs. See [GNUInstallDirs](https://cmake.org/cmake/help/latest/module/GNUInstallDirs.html) for more information. |
| `H5EXAMPLE_DISABLE_COMPILER_WARNINGS` | `BOOL` | `OFF` | If `ON`, disables most or all compiler warnings when building the HDF5 library example programs. |
| `H5EXAMPLE_BUILD_FRAMEWORKS` | `BOOL` | `OFF` | If `ON`, the HDF5 library example programs will be built as a framework bundle when built on MacOS. |

### Filter plugin examples options

These options control building of specific HDF5 filter plugin examples. They have no effect if `H5EXAMPLE_BUILD_FILTERS` is not `ON`.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `ENABLE_BITGROOM` | `BOOL` | `ON` | If `ON`, enables building of the Bitgroom filter plugin examples. |
| `ENABLE_BITROUND` | `BOOL` | `ON` | If `ON`, enables building of the Bitround filter plugin examples. |
| `ENABLE_BLOSC` | `BOOL` | `ON` | If `ON`, enables building of the Blosc filter plugin examples. |
| `ENABLE_BLOSC2` | `BOOL` | `ON` | If `ON`, enables building of the Blosc2 filter plugin examples. |
| `ENABLE_BSHUF` | `BOOL` | `ON` | If `ON`, enables building of the Bitshuffle filter plugin examples. |
| `ENABLE_BZIP2` | `BOOL` | `ON` | If `ON`, enables building of the bzip2 filter plugin examples. |
| `ENABLE_FPZIP` | `BOOL` | `OFF` | If `ON`, enables building of the fpzip filter plugin examples. |
| `ENABLE_JPEG` | `BOOL` | `ON` | If `ON`, enables building of the Jpeg filter plugin examples. |
| `ENABLE_LZ4` | `BOOL` | `ON` | If `ON`, enables building of the lz4 filter plugin examples. |
| `ENABLE_LZF` | `BOOL` | `ON` | If `ON`, enables building of the lzf filter plugin examples. |
| `ENABLE_MAFISC` | `BOOL` | `OFF` | If `ON`, enables building of the mafisc filter plugin examples. |
| `ENABLE_SZ` | `BOOL` | `OFF` | If `ON`, enables building of the sz filter plugin examples. |
| `ENABLE_ZFP` | `BOOL` | `OFF` | If `ON`, enables building of the zfp filter plugin examples. |
| `ENABLE_ZSTD` | `BOOL` | `ON` | If `ON`, enables building of the zstd filter plugin examples. |

## HDF5 testing options

These are options which control how HDF5 testing is built and executed.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `BUILD_TESTING` | `BOOL` | `ON` | If `ON`, enables building of HDF5 test programs and testing of HDF5. |
| `HDF_TEST_EXPRESS` | `STRING` | `3` | Specifies how exhaustive HDF5 testing should be, with smaller values causing more exhaustive testing to be performed. Valid values are `0`, `1`, `2`, `3`, where `0` means to perform exhaustive testing and `3` means to perform the quickest testing. |
| `CTEST_TEST_TIMEOUT` | `STRING` | `1200` | Specifies the maximum amount of time (in seconds) before a test program will be terminated due to a timeout. If modified, `DART_TESTING_TIMEOUT` should be updated as well. |
| `DART_TESTING_TIMEOUT` | `STRING` | `1200` | Specifies the maximum amount of time (in seconds) before a test program will be terminated due to a timeout. If modified, `CTEST_TEST_TIMEOUT` should be updated as well. |
| `HDF5_DISABLE_TESTS_REGEX` | `STRING` | `""` (empty string) | Specifies a regular expression string which can be used to disable specific HDF5 tests with names matching the specified string. For general naming patterns of HDF5 tests, see [INSTALL_CMake.md](./INSTALL_CMake.md#section-xiii). |
| `HDF5_TEST_SERIAL` | `BOOL` | `ON` | If `ON`, enables testing of HDF5's serial (i.e., non-parallel) tests. |
| `HDF5_TEST_PARALLEL` | `BOOL` | `ON` (if `HDF5_ENABLE_PARALLEL` is `ON`) | If `ON`, enables testing of HDF5's parallel tests. |
| `HDF5_TEST_TOOLS` | `BOOL` | `ON` (if `HDF5_BUILD_TOOLS` is `ON`) | If `ON`, enables testing of HDF5's tool programs. |
| `HDF5_TEST_FORTRAN` | `BOOL` | `ON` (if `HDF5_BUILD_FORTRAN` is `ON`) | If `ON`, enables testing of HDF5's Fortran wrapper interface. |
| `HDF5_TEST_JAVA` | `BOOL` | `ON` (if `HDF5_BUILD_JAVA` is `ON`) | If `ON`, enables testing of HDF5's Java wrapper interface. |
| `HDF5_TEST_CPP` | `BOOL` | `ON` (if `HDF5_BUILD_CPP_LIB` is `ON`) | If `ON`, enables testing of HDF5's C++ wrapper interface. |
| `HDF5_TEST_EXAMPLES` | `BOOL` | `ON` (if `HDF5_BUILD_EXAMPLES` is `ON`) | If `ON`, enables testing of HDF5's example programs. |
| `HDF5_TEST_API` | `BOOL` | `ON` | If `ON`, enables testing of HDF5's API tests. |
| `HDF5_TEST_API_INSTALL` | `BOOL` | `OFF` | If `ON`, installs HDF5's API test programs on the system when installing HDF5. Has no effect if `HDF5_TEST_API` is `OFF`. |
| `HDF5_TEST_API_ENABLE_ASYNC` | `BOOL` | `OFF` | If `ON`, enables testing of HDF5's async API tests. Has no effect if `HDF5_TEST_API` is `OFF`. |
| `HDF5_TEST_API_ENABLE_DRIVER` | `BOOL` | `OFF` | If `ON`, enables a driver program that can be used for testing HDF5's API tests as a client against a user-defined server program. Has no effect if `HDF5_TEST_API` is `OFF`. |
| `HDF5_TEST_API_SERVER` | `STRING` | `""` (empty string) | Specifies a server program to use with the HDF5 API test driver program. Has no effect if `HDF5_TEST_API_ENABLE_DRIVER` is `OFF`. |
| `HDF5_TEST_VFD` | `BOOL` | `OFF` | If `ON`, enables testing of several HDF5 Virtual File Drivers. |
| `HDF5_TEST_FHEAP_VFD` | `BOOL` | `ON` (if `HDF5_TEST_VFD` is `ON`) | If `ON`, enables testing of the `fheap` test program with several HDF5 Virtual File Drivers. This test program may take a significant amount of time for specific Virtual File Drivers. |
| `HDF5_TEST_SWMR` | `BOOL` | `ON` | If `ON`, enables testing of the [SWMR](https://support.hdfgroup.org/documentation/hdf5-docs/advanced_topics/intro_SWMR.html) SWMR (Single-Writer / Multiple-Reader) tests. |
| `HDF5_TEST_PASSTHROUGH_VOL` | `BOOL` | `OFF` | If `ON`, enables testing of HDF5's tests with some passthrough VOL connectors. |
| `HDF5_TEST_FHEAP_PASSTHROUGH_VOL` | `BOOL` | `ON` (if `HDF5_TEST_PASSTHROUGH_VOL` is `ON`) | If `ON`, enables testing of the `fheap` test program with some passthrough VOL connectors. This test program may take a significant amount of time for specific passthrough VOL connectors. |
| `HDF5_TEST_SHELL_SCRIPTS` | `BOOL` | `ON` | If `ON`, enables testing of HDF5 shell script tests. These tests currently only run on Unix-like systems. |
| `ENABLE_EXTENDED_TESTS` | `BOOL` | `OFF` | If `ON`, enables some additional HDF5 testing that is not included in standard testing configurations. |
| `CTEST_SUBMIT_RETRY_DELAY` | `STRING` | `20` | Amount of time (in seconds) to wait between timed out submissions when using CTest to submit to a dashboard (see [CDash](https://www.cdash.org/) and [Dashboard Client](https://cmake.org/cmake/help/latest/manual/ctest.1.html#dashboard-client)). |
| `HDF5_USING_ANALYSIS_TOOL` | `BOOL` | `OFF` | If `ON`, indicates that an analysis checker is being used and that some testing should be skipped. |

## Compiler wrapper options

These are options that control how the HDF5 compiler wrapper scripts are built.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_H5CC_C_COMPILER` | `STRING` | C compiler set at configure time for serial HDF5. <br /> MPI C compiler set at configure time for parallel HDF5 (usually `mpicc` or similar). | The program to use for compiling programs with `h5cc`. |
| `HDF5_H5CC_Fortran_COMPILER` | `STRING` | Fortran compiler set at configure time for serial HDF5. <br /> MPI Fortran compiler set at configure time for parallel HDF5 (usually `mpif90` or similar). | The program to use for compiling programs with `h5fc` / `h5pfc`. |
| `HDF5_H5CC_CXX_COMPILER` | `STRING` | C++ compiler set at configure time. | The program to use for compiling programs with `h5c++`. |

## Debugging options

These are options that are typically used for debugging HDF5.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_DEBUG_APIS` | `BOOL` | `ON` if `CMAKE_BUILD_TYPE` is `Developer`; `OFF` otherwise | If `ON`, enables debugging interfaces in HDF5. Debugging HDF5 is described more in [Debugging HDF5 Applications](https://support.hdfgroup.org/documentation/hdf5/latest/_a_p_p_d_b_g.html). |
| `HDF5_ENABLE_DEBUG_H5AC_DIRTY_BYTES` | `BOOL` | `OFF` | If `ON`, enables debugging of dirty bytes in the metadata cache (`H5AC`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5B2` | `BOOL` | `OFF` | If `ON`, enables debugging of the version 2 B-tree (`H5B2`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5C_SANITY_CHECKS` | `BOOL` | `OFF` | If `ON`, enables sanity checking in the cache code (`H5C`). `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5FA` | `BOOL` | `OFF` | If `ON`, enables debugging of the Fixed Array (`H5FA`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5FD_ALLOC` | `BOOL` | `OFF` | If `ON`, enables debugging of the Virtual File Driver (`H5FD`) file space allocation code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5FL` | `BOOL` | `OFF` | If `ON`, enables debugging of the memory free list (`H5FL`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5FS` | `BOOL` | `OFF` | If `ON`, enables debugging of the file free space tracking (`H5FS`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5FS_ASSERT` | `BOOL` | `OFF` | If `ON`, enables debug assertions in the file free space tracking (`H5FS`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5FS_SINFO` | `BOOL` | `OFF` | If `ON`, enables debugging of managed file free space sections. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5MF_AGGR` | `BOOL` | `OFF` | If `ON`, enables debugging of aggregations in the file free space management (`H5MF`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5MF_ALLOC` | `BOOL` | `OFF` | If `ON`, enables debugging of allocations in the file free space management (`H5MF`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5MF_ALLOC_MORE` | `BOOL` | `OFF` | If `ON`, enables more detailed debugging of allocations in the file free space management (`H5MF`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5MF_ALLOC_DUMP` | `BOOL` | `OFF` | If `ON`, enables debug printing of file free space sections in the file free space management (`H5MF`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5R` | `BOOL` | `OFF` | If `ON`, enables debugging of the references (`H5R`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5S_HYPER` | `BOOL` | `OFF` | If `ON`, enables debugging of hyperslab selections in dataspace (`H5S`) objects. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5T_REF` | `BOOL` | `OFF` | If `ON`, enables debugging of references in the datatype (`H5T`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |
| `HDF5_ENABLE_DEBUG_H5TS` | `BOOL` | `OFF` | If `ON`, enables debugging of the library's threading (`H5TS`) code. `CMAKE_BUILD_TYPE` must be `Developer` for this option to be available. |

## Sanitizer, code coverage and formatting options

These options enable sanitizers, code coverage and code formatting tools

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_SANITIZERS` | `BOOL` | `OFF` | If `ON`, enables the option to build HDF5 with several different sanitizers, depending on the compiler. |
| `HDF5_USE_SANITIZER` | `STRING` | `""` (empty string) | Specifies which sanitizer(s) to use when building HDF5. Valid values vary depending on the compiler; see the sanitizers [README.md](../config/sanitizer/README.md) for information on how to set this option. |
| `HDF5_ENABLE_USING_MEMCHECKER` | `BOOL` | `OFF` | If `ON`, indicates that a memory checker such as Valgrind will be used when testing HDF5. This fixes up some code and testing to prevent false positives and other issues. |
| `HDF5_ENABLE_ANALYZER_TOOLS` | `BOOL` | `OFF` | If `ON`, enables the use of some Clang tools, such as `clang-tidy` and `include-what-you-use`. See [README.md](../config/sanitizer/README.md) for information on how to use these tools. |
| `HDF5_ENABLE_COVERAGE` / `CODE_COVERAGE` | `BOOL` | `OFF` | If `ON`, enables the use of code coverage tools. See [README.md](../config/sanitizer/README.md) for information on how to use these tools. Both options should be simultaneously enabled/disabled. |
| `HDF5_ENABLE_FORMATTERS` | `BOOL` | `OFF` | If `ON`, enables code formatting with `clang-format`. |

## Deprecated options

These are options that are deprecated in favor of other options and may be removed in future versions of HDF5.

| CMake option | Type | Default | Description |
|:-------------|:-----|:--------|:------------|
| `HDF5_ENABLE_TRACE` | `BOOL` | `OFF` | Deprecated |
| `HDF5_ENABLE_EMBEDDED_LIBINFO` | `BOOL` | `OFF` | Deprecated |
| `ZLIB_GIT_BRANCH` | `STRING` | `""` (empty string) | Deprecated in favor of `ZLIB_GIT_TAG` |
| `ZLIBNG_GIT_BRANCH` | `STRING` | `""` (empty string) | Deprecated in favor of `ZLIBNG_GIT_TAG` |
| `LIBAEC_GIT_BRANCH` | `STRING` | `""` (empty string) | Deprecated in favor of `LIBAEC_GIT_TAG` |
| `PLUGIN_PACKAGE_NAME` | `STRING` | `""` (empty string) | Deprecated in favor of `HDF5_FILTER_PLUGINS_PACKAGE_NAME` |
| `PLUGIN_TGZ_NAME` | `STRING` | `""` (empty string) | Deprecated in favor of `HDF5_FILTER_PLUGINS_TGZ_NAME` |
| `PLUGIN_TGZ_ORIGPATH` | `STRING` | `""` (empty string) | Deprecated in favor of `HDF5_FILTER_PLUGINS_TGZ_ORIGPATH` |
| `PLUGIN_GIT_URL` | `STRING` | `""` (empty string) | Deprecated in favor of `HDF5_FILTER_PLUGINS_GIT_URL` |
| `PLUGIN_GIT_BRANCH` | `STRING` | `""` (empty string) | Deprecated in favor of `HDF5_FILTER_PLUGINS_GIT_TAG` |

## Unsupported option combinations
<a name="unsupported_combos"></a>

Some HDF5 feature configuration options are incompatible with each other and will cause a configuration error if enabled together without setting the `HDF5_ALLOW_UNSUPPORTED` option to `ON`. These features may or may not work together; they are simply combinations that are not tested or officially supported.

- The parallel HDF5 library, enabled with `HDF5_ENABLE_PARALLEL` set to `ON`, is incompatible with the multi-thread concurrency and thread-safe features, as well as the C++ wrapper interface. Unless `HDF5_ALLOW_UNSUPPORTED` has been specified, the following options must be disabled:

    - `HDF5_ENABLE_CONCURRENCY`
    - `HDF5_ENABLE_THREADSAFE`
    - `HDF5_BUILD_CPP_LIB`

- The multi-thread concurrency (`HDF5_ENABLE_CONCURRENCY`) and thread-safe (`HDF5_ENABLE_THREADSAFE`) features are incompatible with the high-level, Fortran, Java and C++ interfaces, as locking is not hoisted into the higher-level API calls. Unless `HDF5_ALLOW_UNSUPPORTED` has been specified, the following options must be disabled:

    - `HDF5_BUILD_HL_LIB`
    - `HDF5_BUILD_FORTRAN`
    - `HDF5_BUILD_JAVA`
    - `HDF5_BUILD_CPP_LIB`

- The multi-thread concurrency (`HDF5_ENABLE_CONCURRENCY`) and thread-safe (`HDF5_ENABLE_THREADSAFE`) features are mutually exclusive, only one or the other may be enabled.
