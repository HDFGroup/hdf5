# <img src="Cmake_logo.svg" alt="Cmake logo" width=24> CMake Installations

CMake produces the following set of folders; bin, include, lib and share. The LICENSE and CHANGELOG.md file are placed in the share folder.

The bin folder contains the tools and the build scripts. Additionally, CMake creates dynamic versions of the tools with the suffix "-shared".

   ```
   build scripts
   /-------------
   CMake: h5c++, h5cc, h5hlc++, h5hlcc
   ```

The include folder holds the header files and the fortran mod files. CMake places the fortran mod files into separate shared and static subfolders, however the use of `HDF5_INSTALL_MOD_FORTRAN` places one set of mod files into the include folder. Because CMake produces a tools library, the header files for tools will appear in the include folder.

The lib folder contains the library files, and CMake adds the pkgconfig subfolder with the hdf5*.pc files used by the bin/build scripts created by the CMake build. CMake separates the C interface code from the fortran code by creating C-stub libraries for each Fortran library. In addition, only CMake installs the tools library. The names of the szip libraries are different between the build systems.

CMake builds include a number of CMake specific files for support of CMake's find_package and support for the HDF5 Examples CMake project.

# How To Convert Autotools Build Options to CMake Options

## Introduction
The table below shows the equivalent CMake build options that match those from Autotools. See the
release_docs/INSTALL_CMake.txt file for more information on the CMake build system and options.

## Autotools to CMake Options
| Autotools Build Options | CMake Build Options | Notes |
| ------- |  ------- | ------------ |
| warnings-as-errors[default=no] | HDF5_ENABLE_WARNINGS_AS_ERRORS "Interpret some warnings as errors" [OFF] |  |
| build-mode[--enable-build-mode=(debug\|production\|clean)] | CMAKE_BUILD_TYPE "Debug" "Release" "RelWithDebInfo" "MinSizeRel" "Developer" [Release] |  |
| unsupported[Allow unsupported combinations of configure options] | HDF5_ALLOW_UNSUPPORTED "Allow unsupported combinations of configure options" [OFF] |  |
| nonstandard-features[Enable support for non-standard programming language features[default=yes]] | HDF5_ENABLE_NONSTANDARD_FEATURES "Enable support for non-standard programming language features" [ON] |  |
| nonstandard-feature-float16[Enable support for _Float16 C datatype [default=yes]] | HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16 "Enable support for _Float16 C datatype" [${HDF5_ENABLE_NONSTANDARD_FEATURES}] | if (HDF5_ENABLE_NONSTANDARD_FEATURES) |
| static_exec[Install only statically linked executables [default=no]] | BUILD_STATIC_EXECS "Build Static Executables" [OFF] |  |
| sharedlib-rpath[Disable use of the '=Wl,-rpath' linker option] |  |  |
| zlib[Use zlib library for external deflate I/O filter [default=yes]] | HDF5_ENABLE_ZLIB_SUPPORT "Enable Zlib Filters" [OFF] |  |
| szlib[Use szlib library for external szlib I/O filter [default=yes]] | HDF5_ENABLE_SZIP_SUPPORT "Use SZip Filter" [OFF] |  |
| default-api-version[Specify default release version of public symbols [default=v200]] | HDF5_DEFAULT_API_VERSION "v200" CACHE STRING "Enable v2.0 API (v16, v18, v110, v112, v114, v200)" [v200] |  |
| default-plugindir[--with-default-plugindir=location], [Specify default location for plugins [default="/usr/local/hdf5/lib/plugin"]] | H5_DEFAULT_PLUGINDIR “Define the default plugins path” | if (WINDOWS)<br>H5_DEFAULT_PLUGINDIR "%ALLUSERSPROFILE%/hdf5/lib/plugin"<br>else ()<br>H5_DEFAULT_PLUGINDIR "/usr/local/hdf5/lib/plugin"<br>endif () |
| **Autotools Features Options** | **CMake Features Options** | **Notes** |
| dimension-scales-with-new-ref[Use new references when creating dimension scales. [default=no]] | HDF5_DIMENSION_SCALES_NEW_REF "Use new-style references with dimension scale APIs" [OFF] |  |
| map-api[Build the map API (H5M). [default=no]] | HDF5_ENABLE_MAP_API "Build the map API" [OFF] |  |
| subfiling-vfd[Build the subfiling I/O virtual file driver (VFD). Requires --enable-parallel. [default=no]] | HDF5_ENABLE_SUBFILING_VFD "Build Parallel HDF5 Subfiling VFD" [OFF] | if (HDF5_BUILD_UTILS) |
| direct-vfd[Build the direct I/O virtual file driver (VFD). [default=no]] | HDF5_ENABLE_DIRECT_VFD "Build the Direct I/O Virtual File Driver" [OFF] |  |
| mirror-vfd[Build the socket-based Mirror virtual file driver (VFD). [default=no]] | HDF5_ENABLE_MIRROR_VFD "Build the Mirror Virtual File Driver" [OFF] | if (HDF5_BUILD_UTILS) |
| ros3-vfd[Build the Read-Only S3 virtual file driver (VFD). [default=no]] | HDF5_ENABLE_ROS3_VFD "Build the ROS3 Virtual File Driver" [OFF] |  |
| libhdfs[Provide libhdfs library to enable HDFS virtual file driver (VFD) [default=no]] | HDF5_ENABLE_HDFS "Enable HDFS" [OFF] |  |
| threads[Enable threads capability. A prerequisite for enabling threadsafe API calls.  [default=yes] | HDF5_THREADS_ENABLED "Enable thread support" [ON] |  |
| threadsafe[Enable thread-safe capability. Not compatible with the high-level library, Fortran, or C++ wrappers.  [default=no]] | HDF5_ENABLE_THREADSAFE "Enable Threadsafety" [OFF] |  |
| file-locking[Sets the default for whether or not to use file locking when opening files. [default=best-effort]] | HDF5_USE_FILE_LOCKING "Use file locking by default (mainly for SWMR)" [ON] | HDF5_IGNORE_DISABLED_FILE_LOCKS "Ignore file locks when disabled on file system" [ON] |
| enable-concurrency[Support for concurrent multithreaded operation of supported API routines [default=no]] | HDF5_ENABLE_CONCURRENCY "Enable multi-threaded concurrency" [OFF] | This option also provides threadsafe execution of all other, non-concurrent operations. |
| **Autotools Language Options** | **CMake Language Options** | **Notes** |
| fortran[Compile the Fortran interface [default=no]] | HDF5_BUILD_FORTRAN "Build FORTRAN support" [OFF] |  |
| fmoddir[--with-fmoddir=DIR], [Fortran module install directory] | HDF5_INSTALL_MODULE_DIR=$<INSTALL_PREFIX>/mod |  |
| cxx[Compile the C++ interface [default=no]] | HDF5_BUILD_CPP_LIB "Build HDF5 C++ Library" [OFF] |  |
| hl[Enable the high-level library. [default=yes (unless build mode = clean)] | HDF5_BUILD_HL_LIB "Build HIGH Level HDF5 Library" [ON] |  |
| java[Compile the Java JNI interface [default=no]] | HDF5_BUILD_JAVA "Build JAVA support" [OFF] |  |
| parallel[Search for MPI-IO and MPI support files] | HDF5_ENABLE_PARALLEL "Enable parallel build (requires MPI)" [OFF] |  |
| **Autotools Test Options** | **CMake Test Options** | **Notes** |
| tests[Compile the HDF5 tests [default=yes]] | BUILD_TESTING "Build HDF5 Unit Testing" [ON] |  |
| test-express[Set HDF5 testing intensity level (0-3) [0 = exhaustive testing; 3 = quicker testing; default=3] | HDF_TEST_EXPRESS "Control testing framework (0-3)" ["3"] |  |
| tools[Compile the HDF5 tools [default=yes]] | HDF5_BUILD_TOOLS "Build HDF5 Tools" [ON] |  |
| parallel-tools[Enable building parallel tools. [default=no]] | HDF5_BUILD_PARALLEL_TOOLS  "Build Parallel HDF5 Tools" [OFF] |  |
| **Autotools Doc Options** | **CMake Doc Options** | **Notes** |
| doxygen[Compile the HDF5 doxygen files [default=no]] | HDF5_BUILD_DOC "Build documentation" [OFF] |  |
| doxygen-errors[Error on HDF5 doxygen warnings [default=no]] | HDF5_ENABLE_DOXY_WARNINGS "Enable fail if doxygen parsing has warnings." [OFF] |  |
| **Autotools Dev Options** | **CMake Dev Options** | **Notes** |
| sanitize-checks[default=none] | HDF5_USE_SANITIZER ["Compile with a sanitizer. Options are: Address, Memory, MemoryWithOrigins, Undefined, Thread, Leak, 'Address;Undefined', CFI"] | Requires HDF5_ENABLE_SANITIZERS "execute the Clang sanitizer" [OFF] |
| asserts[Determines whether NDEBUG is defined or not, which controls assertions. [default=yes if debug build, otherwise no]] | HDF5_ENABLE_ASSERTS "Determines whether NDEBUG is defined to control assertions." [OFF] |  |
| developer-warnings[Determines whether developer warnings will be emitted. [default=no]] | HDF5_ENABLE_DEV_WARNINGS "Enable HDF5 developer group warnings" [OFF |  |
| show-all-warnings[Enable showing all compiler warnings (for developer debugging). [default=no]] | HDF5_SHOW_ALL_WARNINGS "Show all warnings (i.e. not suppress "noisy" ones internally)" [OFF] |  |
| profiling[Enable profiling flags (e.g.: -pg). [default=no]] | HDF5_ENABLE_PROFILING "Enable profiling flags independently from the build mode." [OFF] |  |
| optimization[Enable optimization flags/settings [default depends on build mode: debug=debug, production=high, clean=none]] | HDF5_ENABLE_OPTIMIZATION "Enable optimization flags/settings independently from the build mode" [OFF] |  |
| diags[Allow default enhanced diagnostics to the build. [default=no]] | HDF5_ENABLE_BUILD_DIAGS "Enable color and URL extended diagnostic messages" [OFF] |  |
| symbols[Add debug symbols to the library (e.g.: build with -g). [default=yes if debug build, otherwise no]] | HDF5_ENABLE_SYMBOLS "Add debug symbols to the library independent of the build mode and optimization level." [OFF] |  |
| internal-debug[Enable extra debugging output on HDF5 library errors. [default=all if debug build, otherwise none]] | HDF5_ENABLE_DEBUG_APIS "Turn on extra debug output in all packages" [OFF] |  |
| trace[Enable HDF5 API tracing capability. [default=yes if debug build, otherwise no]] | HDF5_ENABLE_ASSERTS "Determines whether NDEBUG is defined to control assertions (OFF NO YES)" [OFF] |  |
| using-memchecker[Enable this option if a memory allocation and/or bounds checking tool will be used on the HDF5 library. [default=no]] | HDF5_ENABLE_USING_MEMCHECKER "Indicate that a memory checker is used" [OFF] |  |
| instrument[Enable library instrumentation of optimization tracing (only used with parallel builds). [default=yes if a parallel debug build, otherwise no]] | HDF5_ENABLE_INSTRUMENT "Instrument The library" [OFF] |  |
| dconv-exception[Check exception handling functions during data conversions [default=yes]] | HDF5_WANT_DCONV_EXCEPTION "exception handling functions is checked during data conversions" [ON] |  |
| dconv-accuracy[Guarantee data accuracy during data conversions [default=yes]] | HDF5_WANT_DATA_ACCURACY "IF data accuracy is guaranteed during data conversions" [ON] |  |
| deprecated-symbols[Enable deprecated public API symbols. [default=yes (unless build mode = clean)]] | HDF5_ENABLE_DEPRECATED_SYMBOLS "Enable deprecated public API symbols" [ON] |  |
| strict-format-checks[Enable strict file format checks. [default=yes if debug build, otherwise no]] | HDF5_STRICT_FORMAT_CHECKS "Whether to perform strict file format checks" [OFF] |  |
| preadwrite[Enable using pread/pwrite instead of read/write in sec2/log/core VFDs. [default=yes if pread/pwrite are present]] | HDF5_ENABLE_PREADWRITE "Use pread/pwrite in sec2/log/core VFDs in place of read/write (when available)" [ON] |  |
| embedded-libinfo[Enable embedded library information [default=yes]] | HDF5_ENABLE_EMBEDDED_LIBINFO "Embed library info into executables" [ON] |  |
| **Autotools Options** | **CMake Unused** | **Notes** |
| examplesdir[--with-examplesdir=location], [Specify path for examples [default="DATAROOTDIR/hdf5_examples"]] |  |  |
| libmfu[--with-libmfu=DIR], [Use the libmfu library [default=no]] |  |  |
| pthread[--with-pthread=DIR][Specify alternative path to Pthreads library] | Handled by HDF5_THREADS_ENABLED |  |

