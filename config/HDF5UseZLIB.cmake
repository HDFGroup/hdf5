#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# -----------------------------------------------------------------------------
# HDF5 CMake Zlib(-ng) Support Configuration
# -----------------------------------------------------------------------------
# This CMake module configures support for zlib in HDF5. It provides options
# for enabling/disabling zlib(-ng) support, selecting static/shared builds, and
# controlling how zlib(-ng) is found or built (from system-installed libraries,
# from local source code on the system, or externally via the GIT/TGZ options
# for HDF5_ALLOW_EXTERNAL_SUPPORT).
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# the FetchContent module is needed for building compression libraries from source
# -----------------------------------------------------------------------------
include (FetchContent)

# -----------------------------------------------------------------------------
# Specify major options at the top of the file
# -----------------------------------------------------------------------------
cmake_dependent_option (HDF5_USE_ZLIB_NG "Use zlib-ng library as zlib library" OFF HDF5_ENABLE_ZLIB_SUPPORT OFF)
cmake_dependent_option (HDF5_USE_ZLIB_STATIC "Find static zlib library" OFF HDF5_ENABLE_ZLIB_SUPPORT OFF)
cmake_dependent_option (HDF5_MODULE_MODE_ZLIB "Prefer module mode to find ZLIB" ON "HDF5_ENABLE_ZLIB_SUPPORT;NOT ZLIB_USE_EXTERNAL;NOT HDF5_USE_ZLIB_NG" OFF)
mark_as_advanced (HDF5_MODULE_MODE_ZLIB)
cmake_dependent_option (ZLIB_USE_EXTERNAL "Use External Library Building for ZLIB" OFF HDF5_ENABLE_ZLIB_SUPPORT OFF)
mark_as_advanced (ZLIB_USE_EXTERNAL)
cmake_dependent_option (ZLIB_USE_LOCALCONTENT "Use local file for ZLIB FetchContent" OFF HDF5_ENABLE_ZLIB_SUPPORT OFF)
mark_as_advanced (ZLIB_USE_LOCALCONTENT)

#-----------------------------------------------------------------------------
# ZLib support
#-----------------------------------------------------------------------------
# Function to find zlib on the system with find_package()
function (system_zlib_library)
  set (ZLIB_FOUND FALSE)

  # Set package name for zlib if not already set
  if (NOT DEFINED ZLIB_PACKAGE_NAME)
    set (ZLIB_PACKAGE_NAME "zlib")
  endif ()

  set (Z_PACKAGE_NAME ${ZLIB_PACKAGE_NAME}${HDF_PACKAGE_EXT})
  string (TOUPPER "${Z_PACKAGE_NAME}" Z_PACKAGE_NAME_TOUPPER)
  message (VERBOSE "Filter zlib package name: ${Z_PACKAGE_NAME}")

  if (HDF5_MODULE_MODE_ZLIB)
    # Find Zlib (shared or static) via FindZLIB.cmake
    set (ZLIB_USE_STATIC_LIBS ${HDF5_USE_ZLIB_STATIC})
    find_package (ZLIB MODULE)
  else ()
    # Expect that a correctly built library with CMake config files is available
    if (HDF5_USE_ZLIB_STATIC)
      set (ZLIB_SEARCH_TYPE "static")
    else ()
      set (ZLIB_SEARCH_TYPE "shared")
    endif ()
    find_package (ZLIB NAMES "${Z_PACKAGE_NAME_TOUPPER}" "${Z_PACKAGE_NAME}" CONFIG OPTIONAL_COMPONENTS ${ZLIB_SEARCH_TYPE})
  endif ()

  if (ZLIB_FOUND)
    set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)
    set (H5_ZLIB_INCLUDE_DIR_GEN ${ZLIB_INCLUDE_DIR} PARENT_SCOPE)
    set (H5_ZLIB_INCLUDE_DIRS ${H5_ZLIB_INCLUDE_DIRS} ${ZLIB_INCLUDE_DIR} PARENT_SCOPE)
    set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ZLIB::ZLIB PARENT_SCOPE)

    set (H5_ZLIB_FOUND TRUE PARENT_SCOPE)
  endif ()
endfunction ()

# Function to retrieve zlib from external source (if necessary) and add it to
# the build process
#
# NOTE: This function does NOT patch upstream zlib and will need maintenance for
# any changes in the CMake target names, installed configuration files, etc. in new
# releases.
function (external_zlib_library)
  if (NOT HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT|TGZ")
    message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT must be 'GIT' or 'TGZ' when ZLIB_USE_EXTERNAL is ON (Current setting: ${HDF5_ALLOW_EXTERNAL_SUPPORT})")
  endif ()

  # Setup for FetchContent
  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
    if (ZLIB_USE_LOCALCONTENT)
      message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT '${HDF5_ALLOW_EXTERNAL_SUPPORT}' and ZLIB_USE_LOCALCONTENT options are mutually exclusive")
    endif ()

    set (ZLIB_URL ${ZLIB_GIT_URL})

    if (DEFINED ZLIB_GIT_BRANCH)
      # ZLIB_GIT_BRANCH is deprecated, but still available
      set (ZLIB_TAG ${ZLIB_GIT_BRANCH})
    else ()
      set (ZLIB_TAG ${ZLIB_GIT_TAG})
    endif ()

    message (STATUS "Filter zlib will be built from source ${ZLIB_URL} (tag ${ZLIB_TAG})")

    # Instruct FetchContent to retrieve ZLIB from GIT
    FetchContent_Declare (HDF5_ZLIB
        GIT_REPOSITORY ${ZLIB_URL}
        GIT_TAG ${ZLIB_TAG}
    )
  else () # HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ"
    if (NOT DEFINED TGZPATH)
      set (TGZPATH ${HDF5_SOURCE_DIR})
    endif ()

    if (ZLIB_USE_LOCALCONTENT)
      # Use specified zlib .tgz file from system
      set (ZLIB_URL ${TGZPATH}/${ZLIB_TGZ_NAME})
    else ()
      # Use zlib .tgz file ZLIB_TGZ_NAME downloaded from base URL ZLIB_TGZ_ORIGPATH
      set (ZLIB_URL ${ZLIB_TGZ_ORIGPATH}/${ZLIB_TGZ_NAME})
    endif ()

    if (ZLIB_USE_LOCALCONTENT AND NOT EXISTS "${ZLIB_URL}")
      message (FATAL_ERROR "Filter zlib file ${ZLIB_URL} not found (try setting TGZPATH to a directory containing ${ZLIB_TGZ_NAME})")
    endif ()

    message (STATUS "Filter zlib will be built from source ${ZLIB_URL}")

    # Instruct FetchContent to retrieve ZLIB from .tgz file
    FetchContent_Declare (HDF5_ZLIB
        URL ${ZLIB_URL}
        URL_HASH ""
    )
  endif ()

  # Set zlib shared/static library building based off of preference variable
  if (HDF5_USE_ZLIB_STATIC)
    set (BUILD_SHARED_LIBS OFF)
    set (ZLIB_BUILD_SHARED OFF)
    set (BUILD_STATIC_LIBS ON)
    set (ZLIB_BUILD_STATIC ON)
  else ()
    set (BUILD_SHARED_LIBS ON)
    set (ZLIB_BUILD_SHARED ON)
    set (BUILD_STATIC_LIBS OFF)
    set (ZLIB_BUILD_STATIC OFF)
  endif ()

  # Set zlib options for build
  set (CMAKE_INSTALL_BINDIR ${${HDF5_PACKAGE_NAME}_INSTALL_BIN_DIR})
  set (CMAKE_INSTALL_LIBDIR ${${HDF5_PACKAGE_NAME}_INSTALL_LIB_DIR})
  set (CMAKE_INSTALL_INCLUDEDIR ${${HDF5_PACKAGE_NAME}_INSTALL_INCLUDE_DIR})
  set (CMAKE_INSTALL_DOCDIR ${${HDF5_PACKAGE_NAME}_INSTALL_DOC_DIR})
  set (ZLIB_BUILD_TESTING OFF)
  set (ZLIB_INSTALL ON)

  # Set variables for use in HDF5 CMake configuration file when locating
  # the installed CMake files, as they may not be in the same location as
  # our targets file
  set (${HDF5_PACKAGE_NAME}_ZLIB_INSTALL_NAME "zlib")
  set (${HDF5_PACKAGE_NAME}_ZLIB_INSTALL_NAME "zlib" PARENT_SCOPE)
  set (${HDF5_PACKAGE_NAME}_ZLIB_INSTALL_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/${${HDF5_PACKAGE_NAME}_ZLIB_INSTALL_NAME}" PARENT_SCOPE)

  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT" OR NOT ZLIB_USE_LOCALCONTENT)
    message (VERBOSE "Fetching and configuring filter zlib")
  else ()
    message (VERBOSE "Configuring filter zlib")
  endif ()

  # Make zlib available for the build
  FetchContent_MakeAvailable (HDF5_ZLIB)

  # Hide zlib-ng-specific items from the GUI by default
  mark_as_advanced (ZLIB_BUILD_ADA)
  mark_as_advanced (ZLIB_BUILD_BLAST)
  mark_as_advanced (ZLIB_BUILD_IOSTREAM3)
  mark_as_advanced (ZLIB_BUILD_MINIZIP)
  mark_as_advanced (ZLIB_BUILD_PUFF)
  mark_as_advanced (ZLIB_BUILD_SHARED)
  mark_as_advanced (ZLIB_BUILD_STATIC)
  mark_as_advanced (ZLIB_BUILD_TESTING)
  mark_as_advanced (ZLIB_INSTALL)
  mark_as_advanced (ZLIB_WITH_CRC32VX)
  mark_as_advanced (ZLIB_WITH_GVMAT64)
  mark_as_advanced (ZLIB_WITH_INFBACK9)

  # Set expected target names, based on shared/static preference
  # NOTE: These must be maintained with new releases of upstream zlib in
  # order to avoid having to patch the source when exporting targets.
  if (HDF5_USE_ZLIB_STATIC)
    set (zlib_targets ZLIB::ZLIBSTATIC)
  else ()
    set (zlib_targets ZLIB::ZLIB)
  endif ()
  foreach (zlib_target ${zlib_targets})
    if (NOT TARGET ${zlib_target})
      message (FATAL_ERROR "Expected target ${zlib_target} is missing from build of external zlib")
    endif ()
  endforeach ()

  # Optionally add namespace alias for base non-aliased targets
  if (HDF5_USE_ZLIB_STATIC)
    set (zlib_base_target zlibstatic)
  else ()
    set (zlib_base_target zlib)
  endif ()
  if (HDF_PACKAGE_NAMESPACE AND NOT TARGET ${HDF_PACKAGE_NAMESPACE}${zlib_base_target})
    get_target_property (_aliased_target ${zlib_base_target} ALIASED_TARGET)
    if (_aliased_target)
      add_library (${HDF_PACKAGE_NAMESPACE}${zlib_base_target} ALIAS ${_aliased_target})
    else ()
      add_library (${HDF_PACKAGE_NAMESPACE}${zlib_base_target} ALIAS ${zlib_base_target})
    endif ()
    unset (_aliased_target)
  endif ()

  set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)

  # Set include directories for generated and source headers
  set (H5_ZLIB_INCLUDE_DIR_GEN "${hdf5_zlib_BINARY_DIR}" PARENT_SCOPE)
  set (H5_ZLIB_INCLUDE_DIR "${hdf5_zlib_SOURCE_DIR}" PARENT_SCOPE)
  set (H5_ZLIB_INCLUDE_DIRS ${H5_ZLIB_INCLUDE_DIR_GEN} ${H5_ZLIB_INCLUDE_DIR} PARENT_SCOPE)

  set (H5_ZLIB_LIBRARY "${HDF_PACKAGE_NAMESPACE}${zlib_base_target}")
  set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${H5_ZLIB_LIBRARY} PARENT_SCOPE)

  # If built as a sub-project or if cross-compiling, export all exported
  # targets to the build tree. Append to main targets file but keep
  # namespace from upstream.
  if (HDF5_EXTERNALLY_CONFIGURED OR CMAKE_CROSSCOMPILING)
    # NOTE: The export namespace should be maintained with upstream zlib
    export (
      TARGETS ${zlib_base_target}
      FILE ${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-targets.cmake
      NAMESPACE ZLIB::
      APPEND
    )
  endif ()

  set (H5_ZLIB_FOUND TRUE PARENT_SCOPE)
endfunction ()

# Main logic for zlib support
if (HDF5_ENABLE_ZLIB_SUPPORT AND NOT HDF5_USE_ZLIB_NG)
  set (H5_ZLIB_FOUND FALSE)

  if (H5_ZLIB_HEADER)
    # This project is being called from within another and ZLib is already configured
    set (H5_ZLIB_FOUND TRUE)
  elseif (NOT ZLIB_USE_EXTERNAL)
    # Find zlib on the system
    system_zlib_library ()
  else ()
    # Retrieve zlib from external source (if necessary) and add it to the build process
    # Note that in this case "external" could also mean a local .tgz file on the system
    external_zlib_library ()
  endif ()

  if (H5_ZLIB_FOUND)
    # Set variables used later on in build process
    set (H5_HAVE_FILTER_DEFLATE 1)
    set (H5_HAVE_ZLIB_H 1)
    set (H5_HAVE_LIBZ 1)
    set (HDF5_COMP_INCLUDE_DIRECTORIES "${HDF5_COMP_INCLUDE_DIRECTORIES};${H5_ZLIB_INCLUDE_DIRS}")

    # Set variables for libhdf5.settings (and the H5build_settings string)
    list (APPEND SETTINGS_EXTERNAL_FILTERS "DEFLATE(ZLIB)")

    message (VERBOSE "Filter zlib is ON; H5_ZLIB_HEADER=${H5_ZLIB_HEADER}")
  else ()
    if (ZLIB_USE_EXTERNAL)
      message (FATAL_ERROR "External support for zlib in HDF5 was enabled, but couldn't be processed")
    else ()
      message (FATAL_ERROR "Support for zlib in HDF5 was enabled, but zlib couldn't be found")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# ZLib-ng support
#-----------------------------------------------------------------------------
# Function to find zlib-ng on the system with find_package()
function (system_zlib_ng_library)
  set (ZLIB_FOUND FALSE)

  # Set package name for zlib-ng if not already set
  if (NOT DEFINED ZLIBNG_PACKAGE_NAME)
    set (ZLIBNG_PACKAGE_NAME "ZLIBNG")
  endif ()

  set (Z_PACKAGE_NAME ${ZLIBNG_PACKAGE_NAME}${HDF_PACKAGE_EXT})
  message (VERBOSE "Filter zlib-ng package name: ${Z_PACKAGE_NAME}")

  if (HDF5_USE_ZLIB_STATIC)
    set (ZLIBNG_SEARCH_TYPE "static")
  else ()
    set (ZLIBNG_SEARCH_TYPE "shared")
  endif ()
  set (ZLIBNG_USE_STATIC_LIBS ${HDF5_USE_ZLIB_STATIC})

  # If the package name is left as the default "ZLIBNG", start with our own
  # FindZLIBNG.cmake module that prefers a CONFIG find mode if possible and
  # falls back to MODULE find mode if necessary. This allows locating zlib-ng
  # installations that were built with Autotools, as CMake doesn't currently
  # have a FindZLIBNG module and will only locate CMake-built installations.
  # Note that the default package name is ZLIBNG to not conflict with the
  # actual CMake package name of "zlib-ng". This way, ZLIBNG_PACKAGE_NAME can
  # still be specified as "zlib-ng" to avoid our custom module if desired.
  find_package (${ZLIBNG_PACKAGE_NAME} OPTIONAL_COMPONENTS "${ZLIBNG_SEARCH_TYPE}")

  if (${${ZLIBNG_PACKAGE_NAME}_FOUND})
    if (NOT ${ZLIBNG_PACKAGE_NAME}_ZLIB_COMPAT)
      set (H5_ZLIB_HEADER "zlib-ng.h" PARENT_SCOPE)
    else ()
      set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)

      message (VERBOSE "zlib-ng was built with zlib compatibility")
    endif ()
    set (${ZLIBNG_PACKAGE_NAME}_ZLIB_COMPAT ${${ZLIBNG_PACKAGE_NAME}_ZLIB_COMPAT} PARENT_SCOPE)

    set (H5_ZLIB_INCLUDE_DIR_GEN ${ZLIBNG_INCLUDE_DIRS} PARENT_SCOPE)
    set (H5_ZLIB_INCLUDE_DIRS ${H5_ZLIB_INCLUDE_DIRS} ${ZLIBNG_INCLUDE_DIRS} PARENT_SCOPE)

    if (NOT ZLIBNG_ZLIB_COMPAT)
      if (ZLIBNG_USE_STATIC_LIBS)
        set (_zlibng_target_name "zlib-ng::zlibstatic")
      else ()
        set (_zlibng_target_name "zlib-ng::zlib")
      endif ()
    else ()
      if (ZLIBNG_USE_STATIC_LIBS)
        set (_zlibng_target_name "ZLIB::zlibstatic")
      else ()
        set (_zlibng_target_name "ZLIB::ZLIB")
      endif ()
    endif ()

    if (TARGET ${_zlibng_target_name})
      set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${_zlibng_target_name} PARENT_SCOPE)
      set (H5_ZLIB_FOUND TRUE PARENT_SCOPE)
    else ()
      set (H5_ZLIB_FOUND FALSE PARENT_SCOPE)
    endif ()
  endif ()
endfunction ()

# Function to retrieve zlib-ng from external source (if necessary) and add it to
# the build process
#
# NOTE: This function does NOT patch upstream zlib-ng and will need maintenance for
# any changes in the CMake target names, installed configuration files, etc. in new
# releases.
function (external_zlib_ng_library)
  if (NOT HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT|TGZ")
    message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT must be 'GIT' or 'TGZ' when ZLIB_USE_EXTERNAL is ON (Current setting: ${HDF5_ALLOW_EXTERNAL_SUPPORT})")
  endif ()

  # Setup for FetchContent
  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
    if (ZLIB_USE_LOCALCONTENT)
      message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT '${HDF5_ALLOW_EXTERNAL_SUPPORT}' and ZLIB_USE_LOCALCONTENT options are mutually exclusive")
    endif ()

    set (ZLIBNG_URL ${ZLIBNG_GIT_URL})

    if (DEFINED ZLIBNG_GIT_BRANCH)
      # ZLIBNG_GIT_BRANCH is deprecated, but still available
      set (ZLIBNG_TAG ${ZLIBNG_GIT_BRANCH})
    else ()
      set (ZLIBNG_TAG ${ZLIBNG_GIT_TAG})
    endif ()

    message (STATUS "Filter zlib-ng will be built from source ${ZLIBNG_URL} (tag ${ZLIBNG_TAG})")

    # Instruct FetchContent to retrieve zlib-ng from GIT
    FetchContent_Declare (HDF5_ZLIB
        GIT_REPOSITORY ${ZLIBNG_URL}
        GIT_TAG ${ZLIBNG_TAG}
    )
  else () # HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ"
    if (NOT DEFINED TGZPATH)
      set (TGZPATH ${HDF5_SOURCE_DIR})
    endif ()

    if (ZLIB_USE_LOCALCONTENT)
      # Use specified zlib-ng .tgz file from system
      set (ZLIBNG_URL ${TGZPATH}/${ZLIBNG_TGZ_NAME})
    else ()
      # Use zlib-ng .tgz file ZLIBNG_TGZ_NAME downloaded from base URL ZLIBNG_TGZ_ORIGPATH
      set (ZLIBNG_URL ${ZLIBNG_TGZ_ORIGPATH}/${ZLIBNG_TGZ_NAME})
    endif ()

    if (ZLIB_USE_LOCALCONTENT AND NOT EXISTS "${ZLIBNG_URL}")
      message (FATAL_ERROR "Filter zlib-ng file ${ZLIBNG_URL} not found (try setting TGZPATH to a directory containing ${ZLIBNG_TGZ_NAME})")
    endif ()

    message (STATUS "Filter zlib-ng will be built from source ${ZLIBNG_URL}")

    # Instruct FetchContent to retrieve zlib-ng from .tgz file
    FetchContent_Declare (HDF5_ZLIB
        URL ${ZLIBNG_URL}
        URL_HASH ""
    )
  endif ()

  # Set zlib-ng shared/static library building based off of preference variable
  if (HDF5_USE_ZLIB_STATIC)
    set (BUILD_SHARED_LIBS OFF)
    set (BUILD_STATIC_LIBS ON)
  else ()
    set (BUILD_SHARED_LIBS ON)
    set (BUILD_STATIC_LIBS OFF)
  endif ()

  # Set zlib-ng options for build
  set (CMAKE_INSTALL_BINDIR ${${HDF5_PACKAGE_NAME}_INSTALL_BIN_DIR})
  set (CMAKE_INSTALL_LIBDIR ${${HDF5_PACKAGE_NAME}_INSTALL_LIB_DIR})
  set (CMAKE_INSTALL_INCLUDEDIR ${${HDF5_PACKAGE_NAME}_INSTALL_INCLUDE_DIR})
  set (BUILD_TESTING OFF)

  # Set variables for use in HDF5 CMake configuration file when locating
  # the installed targets file, as it may not be in the same location as
  # our targets file
  if (ZLIB_COMPAT)
    set (${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_NAME "ZLIB")
    set (${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_NAME "ZLIB" PARENT_SCOPE)
    set (${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/${${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_NAME}" PARENT_SCOPE)
  else ()
    set (${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_NAME "zlib-ng")
    set (${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_NAME "zlib-ng" PARENT_SCOPE)
    set (${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/${${HDF5_PACKAGE_NAME}_ZLIBNG_INSTALL_NAME}" PARENT_SCOPE)
  endif ()

  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT" OR NOT ZLIB_USE_LOCALCONTENT)
    message (VERBOSE "Fetching and configuring filter zlib-ng")
  else ()
    message (VERBOSE "Configuring filter zlib-ng")
  endif ()

  # Make zlib-ng available for the build
  FetchContent_MakeAvailable (HDF5_ZLIB)

  # Hide zlib-ng-specific items from the GUI by default
  mark_as_advanced (WITH_AVX512)
  mark_as_advanced (WITH_AVX512VNNI)
  mark_as_advanced (WITH_BENCHMARKS)
  mark_as_advanced (WITH_BENCHMARK_APPS)
  mark_as_advanced (WITH_CODE_COVERAGE)
  mark_as_advanced (WITH_FUZZERS)
  mark_as_advanced (WITH_GTEST)
  mark_as_advanced (WITH_GZFILEOP)
  mark_as_advanced (WITH_MAINTAINER_WARNINGS)
  mark_as_advanced (WITH_NATIVE_INSTRUCTIONS)
  mark_as_advanced (WITH_NEW_STRATEGIES)
  mark_as_advanced (WITH_OPTIM)
  mark_as_advanced (WITH_RUNTIME_CPU_DETECTION)
  mark_as_advanced (WITH_SANITIZER)
  mark_as_advanced (WITH_VPCLMULQDQ)
  mark_as_advanced (ZLIB_ALIASES)
  mark_as_advanced (ZLIB_COMPAT)

  # Set expected target names, based on shared/static preference
  # NOTE: These must be maintained with new releases of upstream zlib-ng in
  # order to avoid having to patch the source when exporting targets.
  if (HDF5_USE_ZLIB_STATIC)
    set (zlib_ng_targets zlib-ng-static)
  else ()
    set (zlib_ng_targets zlib-ng)
  endif ()
  foreach (zlib_ng_target ${zlib_ng_targets})
    if (NOT TARGET ${zlib_ng_target})
      message (FATAL_ERROR "Expected target ${zlib_ng_target} is missing from build of external zlib-ng")
    endif ()
  endforeach ()

  # Optionally add namespace alias for targets.  zlib-ng exports some of
  # its public target names (e.g. zlib-ng-static) as aliases to the real
  # underlying target; CMake refuses add_library(NEW ALIAS EXISTING)
  # when EXISTING is itself an alias (aliases cannot chain).  Resolve
  # through ALIASED_TARGET before creating the namespaced alias.
  if (HDF_PACKAGE_NAMESPACE)
    foreach (zlib_ng_target ${zlib_ng_targets})
      if (NOT TARGET ${HDF_PACKAGE_NAMESPACE}${zlib_ng_target})
        get_target_property (_aliased_target ${zlib_ng_target} ALIASED_TARGET)
        if (_aliased_target)
          add_library (${HDF_PACKAGE_NAMESPACE}${zlib_ng_target} ALIAS ${_aliased_target})
        else ()
          add_library (${HDF_PACKAGE_NAMESPACE}${zlib_ng_target} ALIAS ${zlib_ng_target})
        endif ()
        unset (_aliased_target)
      endif ()
    endforeach ()
  endif ()

  # Set the correct header for zlib-ng compatibility
  if (ZLIB_COMPAT)
    set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)
  else ()
    set (H5_ZLIB_HEADER "zlib-ng.h" PARENT_SCOPE)
  endif ()

  # Set include directories for generated and source headers
  set (H5_ZLIB_INCLUDE_DIR_GEN "${hdf5_zlib_BINARY_DIR}" PARENT_SCOPE)
  set (H5_ZLIB_INCLUDE_DIR "${hdf5_zlib_SOURCE_DIR}" PARENT_SCOPE)
  set (H5_ZLIB_INCLUDE_DIRS ${H5_ZLIB_INCLUDE_DIR_GEN} ${H5_ZLIB_INCLUDE_DIR} PARENT_SCOPE)

  if (HDF5_USE_ZLIB_STATIC)
    set (H5_ZLIB_LIBRARY "${HDF_PACKAGE_NAMESPACE}zlib-ng-static")
  else ()
    set (H5_ZLIB_LIBRARY "${HDF_PACKAGE_NAMESPACE}zlib-ng")
  endif ()
  set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${H5_ZLIB_LIBRARY} PARENT_SCOPE)

  # If built as a sub-project or if cross-compiling, export all exported
  # targets to the build tree. Append to main targets file but keep
  # namespace from upstream.
  if (HDF5_EXTERNALLY_CONFIGURED OR CMAKE_CROSSCOMPILING)
    # NOTE: The export namespace should be maintained with upstream zlib-ng
    if (ZLIB_COMPAT)
      set (zlib_ng_export_name ZLIB)
    else ()
      set (zlib_ng_export_name zlib-ng)
    endif ()
    export (
      TARGETS ${zlib_ng_targets}
      FILE ${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-targets.cmake
      NAMESPACE ${zlib_ng_export_name}::
      APPEND
    )
  endif ()

  set (H5_ZLIB_FOUND TRUE PARENT_SCOPE)
endfunction ()

# Main logic for zlib-ng support
if (HDF5_ENABLE_ZLIB_SUPPORT AND HDF5_USE_ZLIB_NG)
  set (H5_ZLIB_FOUND FALSE)

  if (HDF5_MODULE_MODE_ZLIB)
    message (FATAL_ERROR "HDF5_USE_ZLIB_NG and HDF5_MODULE_MODE_ZLIB options are mutually exclusive")
  endif ()

  if (H5_ZLIB_HEADER)
    # This project is being called from within another and ZLib is already configured
    set (H5_ZLIB_FOUND TRUE)
  elseif (NOT ZLIB_USE_EXTERNAL)
    # Find zlib-ng on the system
    system_zlib_ng_library ()
  else ()
    # Retrieve zlib-ng from external source (if necessary) and add it to the build process
    # Note that in this case "external" could also mean a local .tgz file on the system
    external_zlib_ng_library ()
  endif ()

  if (H5_ZLIB_FOUND)
    # Set variables used later on in build process
    set (H5_HAVE_FILTER_DEFLATE 1)
    set (H5_HAVE_ZLIB_H 1)
    if (NOT ZLIB_COMPAT AND NOT ZLIBNG_ZLIB_COMPAT)
      set (H5_HAVE_ZLIBNG_H 1)
    endif ()
    set (H5_HAVE_LIBZ 1)
    set (HDF5_COMP_INCLUDE_DIRECTORIES "${HDF5_COMP_INCLUDE_DIRECTORIES};${H5_ZLIB_INCLUDE_DIRS}")

    # Set variables for libhdf5.settings (and the H5build_settings string)
    if (ZLIB_COMPAT OR ZLIBNG_ZLIB_COMPAT)
      list (APPEND SETTINGS_EXTERNAL_FILTERS "DEFLATE(ZLIB-NG WITH ZLIB COMPAT MODE)")
    else ()
      list (APPEND SETTINGS_EXTERNAL_FILTERS "DEFLATE(ZLIB-NG)")
    endif ()

    message (VERBOSE "Filter zlib-ng is ON; H5_ZLIB_HEADER=${H5_ZLIB_HEADER}")
  else ()
    if (ZLIB_USE_EXTERNAL)
      message (FATAL_ERROR "External support for zlib-ng in HDF5 was enabled, but couldn't be processed")
    else ()
      message (FATAL_ERROR "Support for zlib-ng in HDF5 was enabled, but zlib-ng couldn't be found")
    endif ()
  endif ()
endif ()
