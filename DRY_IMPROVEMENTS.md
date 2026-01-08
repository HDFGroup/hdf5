# DRY (Don't Repeat Yourself) Improvements

This document describes improvements made to eliminate code duplication in the version management system.

## 1. Centralized H5public.h Version Parsing

### Problem
The logic to parse version numbers from H5public.h using regex was duplicated in two locations:
- `config/cmake/scripts/HDF5config.cmake` (lines 47-57)
- `config/examples/HDF5AsSubdirMacros.cmake` (lines 24-34)

This created a maintenance burden where changes to the parsing logic or H5public.h format would require updates in multiple places.

### Solution
Created a shared CMake module that centralizes the version parsing logic:

**New file:** `config/cmake/HDF5VersionParsing.cmake`
- Provides `parse_hdf5_version()` macro
- Handles all regex parsing in one place
- Includes comprehensive error checking
- Well-documented with CMake RST-style documentation

**Updated files:**
- `config/cmake/scripts/HDF5config.cmake` - Now includes and uses the shared module
- `config/examples/HDF5AsSubdirMacros.cmake` - Now includes and uses the shared module

### Benefits
- Single source of truth for version parsing logic
- If H5public.h format changes, only one file needs updating
- Consistent error handling across all usage locations
- Better maintainability and testability

### Example Usage
```cmake
include(${CMAKE_SOURCE_DIR}/config/cmake/HDF5VersionParsing.cmake)
parse_hdf5_version("${CMAKE_SOURCE_DIR}/src/H5public.h"
                   MAJOR_VAR H5_VERS_MAJOR
                   MINOR_VAR H5_VERS_MINOR
                   RELEASE_VAR H5_VERS_RELEASE
                   SUBRELEASE_VAR H5_VERS_SUBRELEASE)
```

## 2. Consolidated Java Version Template

### Problem
Two identical template files existed:
- `java/src-jni/hdf/hdf5lib/H5Version.java.in`
- `java/hdf/hdf5lib/H5Version.java.in`

This created a maintenance burden where developers might update one template but forget the other, leading to inconsistencies.

### Solution
Created a single shared template in a common location:

**New file:** `config/cmake/templates/H5Version.java.in`
- Single source template for H5Version.java generation
- Used by both JNI and FFM Java builds

**Removed files:**
- `java/src-jni/hdf/hdf5lib/H5Version.java.in` (moved to shared location)
- `java/hdf/hdf5lib/H5Version.java.in` (deleted - was duplicate)

**Updated files:**
- `java/src-jni/hdf/hdf5lib/CMakeLists.txt` - Points to shared template
- `java/hdf/hdf5lib/CMakeLists.txt` - Points to shared template

### Benefits
- Single template to maintain
- Impossible to have inconsistencies between JNI and FFM builds
- Clearer separation of concerns (templates in config/, not in source directories)
- Easier to find and update template files

### Template Location
```
config/cmake/templates/
└── H5Version.java.in
```

### CMake Configuration
Both Java CMakeLists.txt files now use:
```cmake
configure_file (
    ${HDF5_SOURCE_DIR}/config/cmake/templates/H5Version.java.in
    ${CMAKE_CURRENT_BINARY_DIR}/H5Version.java
    @ONLY
)
```

## Summary

These improvements reduce duplication and maintenance burden while improving code quality:

| Area | Before | After | Reduction |
|------|--------|-------|-----------|
| Version parsing logic | 2 copies | 1 shared module | 50% |
| Java version templates | 2 identical files | 1 shared template | 50% |

Both improvements follow the DRY principle and make the codebase more maintainable and less error-prone.
