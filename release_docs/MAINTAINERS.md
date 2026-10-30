# HDF5 Library maintainer's notes

This document is a catch-all file with general notes for HDF5 library maintainers.

---

## Table of Contents

* [Updating the installed `hdf5-config.cmake` CMake configuration file](#updating-the-installed-hdf5-config-cmake-cmake-configuration-file)
    * [CMake variables for new features](#cmake-variables-for-new-features)
    * [PRIVATE dependencies](#private-dependencies)
    * [Public `hdf5::` target names](#public-hdf5-target-names)
* [Maintaining external libraries built with CMake FetchContent](#maintaining-external-libraries-built-with-cmake-fetchcontent)
    * [Updating libraries to new versions](#updating-libraries-to-new-versions)

---

## Updating the installed `hdf5-config.cmake` CMake configuration file

### CMake variables for new features

When a new feature is added to the library, developers should consider whether the ability to check
for the existence of that feature should be programmatically available in the CMake logic for a
project which uses HDF5. If so, an entry should be added under the "User Options" section such that
the enabled/disabled status of that feature will be reflected in the installed file.

### PRIVATE dependencies

When `target_link_libraries()` is used to add linking against a library with `PRIVATE` scope for a
particular CMake target exposed by the installed `hdf5-config.cmake`, that dependency may need to be
propagated as a transitive link requirement with a call to `find_dependency()` if the target is a
static library. Otherwise, a CMake project trying to use that target may fail during configuration
with an error similar to:

```CMake
The link interface of target "hdf5-static" contains:

  ZLIB::ZLIB

but the target was not found.  Possible reasons include:

  * There is a typo in the target name.
  * A find_package call is missing for an IMPORTED target.
  * An ALIAS target is missing.
```

Failing to propagate these dependencies creates a situation where the CMake project using HDF5 has
to know ahead of time which `find_package()` calls it needs to include in its own logic to satisfy the
requirements of HDF5's CMake targets. For testing, developers should use a standalone CMake project
which tries linking against one of an HDF5 installation's static library CMake targets that links
against the `PRIVATE` library.

### Public `hdf5::` target names

The linkage-agnostic names such as `hdf5::hdf5` and `hdf5::hdf5_hl` are currently ALIAS targets onto
the linkage-qualified targets (`hdf5-shared`, `hdf5-static`, etc.). Since ALIAS targets can't be
exported, `hdf5-config.cmake` creates them itself after loading the exported targets, using the
linkage it resolved for that `find_package()` call. They are not defined within HDF5's own build.

If these names become real targets in the future, for example when the deprecated linkage-qualified
names are removed, they should be exported the same way the current library targets are: installed
into the `${HDF5_EXPORTED_TARGETS}` export set used by `install (EXPORT ...)`, and added to
`HDF5_LIBRARIES_TO_EXPORT` for the build-tree `export (TARGETS ...)` (see
[CMakeInstallation.cmake](../CMakeInstallation.cmake)). The block that creates the aliases should then
be removed from `hdf5-config.cmake.in`.

Note that a target defined in the build can't have `::` in its name, so `hdf5::hdf5` would be the
exported name of a target called `hdf5`. The existing export set prefixes names with
`HDF_PACKAGE_NAMESPACE`, which is empty by default but set to `hdf5::` by the `ci-StdShar` presets
and `config/cmake/cacheinit.cmake`, so the public names would need their own fixed `hdf5::` namespace
rather than that variable.

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
