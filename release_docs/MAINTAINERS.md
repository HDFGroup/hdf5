# HDF5 Library maintainer's notes

This document is a catch-all file with general notes for HDF5 library maintainers.

---

## Table of Contents

* [Maintaining external libraries built with CMake FetchContent](#maintaining-external-libraries-built-with-cmake-fetchcontent)
    * [Updating libraries to new versions](#updating-libraries-to-new-versions)

---

## Maintaining external libraries built with CMake FetchContent

### Updating libraries to new versions

When performing a new release of HDF5, external libraries that can be fetched and built with CMake's FetchContent should be checked and updated as necessary. For filter libraries, this involves the following steps:

  - For each library being updated:
    - Modify [CacheURLs.cmake](../config/CacheURLs.cmake) and [CMakePresets.json](../CMakePresets.json) to set the new version of the library, git/tgz URL, git tag, etc.
    - Check the library's source code for any change in CMake logic, including target names and export namespaces, installed configuration files, etc. and make adjustments to the CMake logic in HDF5 that sets up to build that library
    - If the library's source is being patched by HDF5, check for any adjustments that need to be made to the file(s) being patched and create a new version, ideally with the source version in the file name, to be used by the patching process
    - Check the library's source code for any new CMake options or settings that should be enabled/disabled or set near the relevant `FetchContent_MakeAvailable()` call in the CMake logic that will fetch and configure the source
    - Check the library's source code for any new CMake options or settings that should be marked as advanced with `mark_as_advanced()` _after_ the relevant `FetchContent_MakeAvailable()` call in order to hide them from CMake GUI programs and prevent a clutter of build options
  - Test building and installing HDF5 with the updated libraries being obtained with FetchContent to check for any new issues
