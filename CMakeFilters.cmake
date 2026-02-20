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
# HDF5 CMake Filter Support Configuration
# -----------------------------------------------------------------------------
# This CMake module configures support for compression filters in HDF5, specifically
# ZLIB (including zlib-ng) and SZIP (libaec). It provides options for enabling/disabling
# filter support, selecting static/shared builds, and controlling how dependencies
# are found or built (from system-installed libraries, from local source code on
# the system, or externally via the GIT/TGZ options for HDF5_ALLOW_EXTERNAL_SUPPORT).
#
# Key Features:
# - Options to enable/disable ZLIB and SZIP support, and select static/shared linking.
# - Support for using zlib-ng as a drop-in replacement for zlib.
# - Support for building dependencies externally (via GIT or TGZ) or using system libraries.
# - Handles configuration of include directories, library targets, and CMake variables
#   for downstream use.
# - Sets up required variables for HDF5 to use the DEFLATE and SZIP filters.
#
# See comments throughout for details on each option and logic branch.
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

cmake_dependent_option (HDF5_USE_LIBAEC_STATIC "Find static AEC library" OFF HDF5_ENABLE_SZIP_SUPPORT OFF)
cmake_dependent_option (HDF5_ENABLE_SZIP_ENCODING "Use SZip Encoding" ON HDF5_ENABLE_SZIP_SUPPORT OFF)
cmake_dependent_option (SZIP_USE_EXTERNAL "Use External Library Building for SZIP" OFF HDF5_ENABLE_SZIP_SUPPORT OFF)
mark_as_advanced (SZIP_USE_EXTERNAL)
cmake_dependent_option (LIBAEC_USE_LOCALCONTENT "Use local file for LIBAEC FetchContent" OFF HDF5_ENABLE_SZIP_SUPPORT OFF)
mark_as_advanced (LIBAEC_USE_LOCALCONTENT)

unset (SETTINGS_EXTERNAL_FILTERS)

#-----------------------------------------------------------------------------
# ZLib support
#-----------------------------------------------------------------------------
# Function to find zlib(-ng) on the system with find_package()
function (system_zlib_library)
  set (ZLIB_FOUND FALSE)

  if (NOT DEFINED ZLIB_PACKAGE_NAME)
    message (FATAL_ERROR "ZLIB_PACKAGE_NAME is undefined")
  endif ()
  if (NOT DEFINED ZLIBNG_PACKAGE_NAME)
    message (FATAL_ERROR "ZLIBNG_PACKAGE_NAME is undefined")
  endif ()
  if (HDF5_USE_ZLIB_NG AND HDF5_MODULE_MODE_ZLIB)
    message (FATAL_ERROR "HDF5_USE_ZLIB_NG and HDF5_MODULE_MODE_ZLIB options are mutually exclusive")
  endif ()

  if (HDF5_USE_ZLIB_NG)
    set (Z_PACKAGE_NAME ${ZLIBNG_PACKAGE_NAME}${HDF_PACKAGE_EXT})
  else ()
    set (Z_PACKAGE_NAME ${ZLIB_PACKAGE_NAME}${HDF_PACKAGE_EXT})
  endif ()
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
    if (HDF5_USE_ZLIB_NG)
      set (H5_ZLIB_HEADER "zlib-ng.h" PARENT_SCOPE)
    else ()
      set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)
    endif ()
    set (H5_ZLIB_INCLUDE_DIR_GEN ${ZLIB_INCLUDE_DIR} PARENT_SCOPE)
    set (H5_ZLIB_INCLUDE_DIRS ${H5_ZLIB_INCLUDE_DIRS} ${ZLIB_INCLUDE_DIR} PARENT_SCOPE)
    set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ZLIB::ZLIB PARENT_SCOPE)

    set (H5_ZLIB_FOUND TRUE PARENT_SCOPE)
  endif ()
endfunction ()

# Function to retrieve zlib(-ng) from external source (if necessary) and add it to the build process
function (external_zlib_library)
  if (NOT HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT|TGZ")
    message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT must be 'GIT' or 'TGZ' when ZLIB_USE_EXTERNAL is ON (Current setting: ${HDF5_ALLOW_EXTERNAL_SUPPORT})")
  endif ()

  # Select the correct folder for ZLIB or ZLIB-NG patching process
  if (HDF5_USE_ZLIB_NG)
    set (zlib_folder "ZLIBNG")
  else ()
    set (zlib_folder "ZLIB")
  endif ()

  # Setup for FetchContent
  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
    if (ZLIB_USE_LOCALCONTENT)
      message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT '${HDF5_ALLOW_EXTERNAL_SUPPORT}' and ZLIB_USE_LOCALCONTENT options are mutually exclusive")
    endif ()

    if (HDF5_USE_ZLIB_NG)
      set (ZLIB_URL ${ZLIBNG_GIT_URL} CACHE STRING "Path to zlib-ng git repository")
      set (ZLIB_BRANCH ${ZLIBNG_GIT_BRANCH})
    else ()
      set (ZLIB_URL ${ZLIB_GIT_URL} CACHE STRING "Path to zlib git repository")
      set (ZLIB_BRANCH ${ZLIB_GIT_BRANCH})
    endif ()

    # Use a different CMakeLists for 'develop' branch to patch zlib
    if (${ZLIB_BRANCH} MATCHES "develop")
      set (ZLIB_FILE "devCMakeLists")
    else ()
      set (ZLIB_FILE "CMakeLists")
    endif ()

    # Instruct FetchContent to retrieve ZLIB from GIT and patch CMakeLists.txt
    FetchContent_Declare (HDF5_ZLIB
        GIT_REPOSITORY ${ZLIB_URL}
        GIT_TAG ${ZLIB_BRANCH}
        PATCH_COMMAND ${CMAKE_COMMAND} -E copy
            ${HDF_RESOURCES_DIR}/${zlib_folder}/${ZLIB_FILE}.txt
            <SOURCE_DIR>/CMakeLists.txt
    )
  else () # HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ"
    if (NOT DEFINED TGZPATH)
      set (TGZPATH ${HDF5_SOURCE_DIR})
    endif ()

    if (HDF5_USE_ZLIB_NG)
      if (ZLIB_USE_LOCALCONTENT)
        # Use specified zlib-ng .tgz file from system
        set (ZLIB_URL ${TGZPATH}/${ZLIBNG_TGZ_NAME})
      else ()
        # Use zlib-ng .tgz file ZLIBNG_TGZ_NAME downloaded from base URL ZLIBNG_TGZ_ORIGPATH
        set (ZLIB_URL ${ZLIBNG_TGZ_ORIGPATH}/${ZLIBNG_TGZ_NAME})
      endif ()
    else ()
      if (ZLIB_USE_LOCALCONTENT)
        # Use specified zlib .tgz file from system
        set (ZLIB_URL ${TGZPATH}/${ZLIB_TGZ_NAME})
      else ()
        # Use zlib .tgz file ZLIB_TGZ_NAME downloaded from base URL ZLIB_TGZ_ORIGPATH
        set (ZLIB_URL ${ZLIB_TGZ_ORIGPATH}/${ZLIB_TGZ_NAME})
      endif ()
    endif ()

    if (ZLIB_USE_LOCALCONTENT AND NOT EXISTS "${ZLIB_URL}")
      message (FATAL_ERROR "Filter ZLIB file ${ZLIB_URL} not found")
    endif ()

    # Instruct FetchContent to retrieve ZLIB from .tgz file and patch CMakeLists.txt
    FetchContent_Declare (HDF5_ZLIB
        URL ${ZLIB_URL}
        URL_HASH ""
        PATCH_COMMAND ${CMAKE_COMMAND} -E copy
            ${HDF_RESOURCES_DIR}/${zlib_folder}/CMakeLists.txt
            <SOURCE_DIR>/CMakeLists.txt
    )
  endif ()

  message (VERBOSE "Filter HDF5_ZLIB will be built from source ${ZLIB_URL}")

  # Make ZLIB available for the build
  FetchContent_MakeAvailable(HDF5_ZLIB)

  # Optionally add namespace alias for static zlib
  if (HDF_PACKAGE_NAMESPACE)
    add_library (${HDF_PACKAGE_NAMESPACE}zlib-static ALIAS zlib-static)
  endif ()
  set (H5_ZLIB_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}zlib-static")

  # Set the correct header for zlib-ng compatibility
  if (HDF5_USE_ZLIB_NG)
    if (ZLIB_COMPAT)
      set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)
    else ()
      set (H5_ZLIB_HEADER "zlib-ng.h" PARENT_SCOPE)
    endif ()
  else ()
    set (H5_ZLIB_HEADER "zlib.h" PARENT_SCOPE)
  endif ()

  # Set include directories for generated and source headers
  set (H5_ZLIB_INCLUDE_DIR_GEN "${hdf5_zlib_BINARY_DIR}" PARENT_SCOPE)
  set (H5_ZLIB_INCLUDE_DIR "${hdf5_zlib_SOURCE_DIR}" PARENT_SCOPE)
  set (H5_ZLIB_INCLUDE_DIRS ${H5_ZLIB_INCLUDE_DIR_GEN} ${H5_ZLIB_INCLUDE_DIR} PARENT_SCOPE)

  set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${H5_ZLIB_STATIC_LIBRARY} PARENT_SCOPE)

  set (H5_ZLIB_FOUND TRUE PARENT_SCOPE)
endfunction ()

# Main logic for zlib(-ng) support
if (HDF5_ENABLE_ZLIB_SUPPORT)
  set (H5_ZLIB_FOUND FALSE)

  # Set package names for zlib and zlib-ng if not already set
  if (NOT DEFINED ZLIB_PACKAGE_NAME)
    set (ZLIB_PACKAGE_NAME "zlib")
  endif ()
  if (NOT DEFINED ZLIBNG_PACKAGE_NAME)
    set (ZLIBNG_PACKAGE_NAME "zlib-ng")
  endif ()

  if (H5_ZLIB_HEADER)
    # This project is being called from within another and ZLib is already configured
    set (H5_ZLIB_FOUND TRUE)
  elseif (NOT ZLIB_USE_EXTERNAL)
    # Find zlib(-ng) on the system
    system_zlib_library ()
  else ()
    # Retrieve zlib(-ng) from external source (if necessary) and add it to the build process
    # Note that in this case "external" could also mean a local .tgz file on the system
    external_zlib_library ()
  endif ()

  if (H5_ZLIB_FOUND)
    # Set variables used later on in build process
    set (H5_HAVE_FILTER_DEFLATE 1)
    set (H5_HAVE_ZLIB_H 1)
    if (HDF5_USE_ZLIB_NG AND NOT ZLIB_COMPAT)
      set (H5_HAVE_ZLIBNG_H 1)
    endif ()
    set (H5_HAVE_LIBZ 1)
    set (HDF5_COMP_INCLUDE_DIRECTORIES "${HDF5_COMP_INCLUDE_DIRECTORIES};${H5_ZLIB_INCLUDE_DIRS}")

    # Set variables for libhdf5.settings (and the H5build_settings string)
    list (APPEND SETTINGS_EXTERNAL_FILTERS "DEFLATE")

    message (VERBOSE "Filter ${ZLIB_PACKAGE_NAME} is ON; H5_ZLIB_HEADER=${H5_ZLIB_HEADER}")
  else ()
    if (ZLIB_USE_EXTERNAL)
      message (FATAL_ERROR "External support for ${ZLIB_PACKAGE_NAME} in HDF5 was enabled, but couldn't be processed")
    else ()
      message (FATAL_ERROR "Support for ${ZLIB_PACKAGE_NAME} in HDF5 was enabled, but ${ZLIB_PACKAGE_NAME} couldn't be found")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# SZIP support
#-----------------------------------------------------------------------------
# Function to find libaec/szip on the system with find_package()
function (system_szip_library)
  set (SZIP_FOUND FALSE)

  if (NOT DEFINED LIBAEC_PACKAGE_NAME)
    message (FATAL_ERROR "LIBAEC_PACKAGE_NAME is undefined")
  endif ()

  if (HDF5_USE_LIBAEC_STATIC)
    set (LIBAEC_SEARCH_TYPE "static")
  else ()
    set (LIBAEC_SEARCH_TYPE "shared")
  endif ()
  set (libaec_USE_STATIC_LIBS ${HDF5_USE_LIBAEC_STATIC})

  # For "libaec", start with our own Findlibaec.cmake module that prefers a
  # CONFIG find mode if possible and falls back to MODULE find mode if necessary.
  # For "szip", use CMake's standard MODULE find mode followed by a CONFIG find
  # mode.
  find_package (${LIBAEC_PACKAGE_NAME} OPTIONAL_COMPONENTS ${LIBAEC_SEARCH_TYPE})

  if (${${LIBAEC_PACKAGE_NAME}_FOUND})
    set (H5_SZIP_INCLUDE_DIR_GEN ${SZIP_INCLUDE_DIR} PARENT_SCOPE)
    set (H5_SZIP_INCLUDE_DIRS ${H5_SZIP_INCLUDE_DIRS} ${SZIP_INCLUDE_DIR} PARENT_SCOPE)

    if (LIBAEC_PACKAGE_NAME MATCHES "libaec")
      # When using libaec to replace SZIP, we require that the libsz compatibility library is available
      if (NOT libsz_FOUND)
        message (FATAL_ERROR "Libaec support in HDF5 was enabled and libaec was found, but no libsz compatibility library was found")
      endif ()

      set (LINK_COMP_LIBS ${LINK_COMP_LIBS} libaec::sz libaec::aec PARENT_SCOPE)
    else ()
      set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${SZIP_LIBRARIES} PARENT_SCOPE)
    endif ()

    set (H5_SZIP_FOUND TRUE PARENT_SCOPE)
  endif ()
endfunction ()

# Function to retrieve libaec/szip from external source (if necessary) and add it to the build process
function (external_szip_library)
  if (NOT HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT|TGZ")
    message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT must be 'GIT' or 'TGZ' when SZIP_USE_EXTERNAL is ON (Current setting: ${HDF5_ALLOW_EXTERNAL_SUPPORT})")
  endif ()

  # Setup for FetchContent
  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
    if (LIBAEC_USE_LOCALCONTENT)
      message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT '${HDF5_ALLOW_EXTERNAL_SUPPORT}' and LIBAEC_USE_LOCALCONTENT options are mutually exclusive")
    endif ()

    set (SZIP_URL ${LIBAEC_GIT_URL} CACHE STRING "Path to szip git repository")
    set (SZIP_BRANCH ${LIBAEC_GIT_BRANCH})

    # Instruct FetchContent to retrieve libaec from GIT and patch CMakeLists.txt
    FetchContent_Declare (SZIP
        GIT_REPOSITORY ${SZIP_URL}
        GIT_TAG ${SZIP_BRANCH}
        PATCH_COMMAND ${CMAKE_COMMAND} -E copy
            ${HDF_RESOURCES_DIR}/LIBAEC/CMakeLists.txt
            <SOURCE_DIR>/CMakeLists.txt
    )
  else () # HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ"
    if (NOT DEFINED TGZPATH)
      set (TGZPATH ${HDF5_SOURCE_DIR})
    endif ()

    if (LIBAEC_USE_LOCALCONTENT)
      # Use specified libaec .tgz file from system
      set (SZIP_URL ${TGZPATH}/${LIBAEC_TGZ_NAME})
    else ()
      # Use libaec .tgz file LIBAEC_TGZ_NAME downloaded from base URL LIBAEC_TGZ_ORIGPATH
      set (SZIP_URL ${LIBAEC_TGZ_ORIGPATH}/${LIBAEC_TGZ_NAME})
    endif ()

    if (LIBAEC_USE_LOCALCONTENT AND NOT EXISTS "${SZIP_URL}")
      message (FATAL_ERROR "Filter SZIP file ${SZIP_URL} not found")
    endif ()

    # Instruct FetchContent to retrieve libaec from .tgz file and patch CMakeLists.txt
    FetchContent_Declare (SZIP
        URL ${SZIP_URL}
        URL_HASH ""
        PATCH_COMMAND ${CMAKE_COMMAND} -E copy
            ${HDF_RESOURCES_DIR}/LIBAEC/CMakeLists.txt
            <SOURCE_DIR>/CMakeLists.txt
    )
  endif ()

  message (VERBOSE "Filter libaec will be built from source ${SZIP_URL}")

  # Make SZIP (libaec) available for the build
  FetchContent_MakeAvailable(SZIP)

  # Optionally add namespace aliases for static szaec and aec
  if (HDF_PACKAGE_NAMESPACE)
    add_library (${HDF_PACKAGE_NAMESPACE}szaec-static ALIAS szaec-static)
    add_library (${HDF_PACKAGE_NAMESPACE}aec-static ALIAS aec-static)
  endif ()
  set (H5_SZIP_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}szaec-static;${HDF_PACKAGE_NAMESPACE}aec-static")

  # Set include directories for generated and source headers
  set (H5_SZIP_INCLUDE_DIR_GEN "${szip_BINARY_DIR}" PARENT_SCOPE)
  set (H5_SZIP_INCLUDE_DIR "${szip_SOURCE_DIR}/include" PARENT_SCOPE)
  set (H5_SZIP_INCLUDE_DIRS ${H5_SZIP_INCLUDE_DIR_GEN} ${H5_SZIP_INCLUDE_DIR} PARENT_SCOPE)

  set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${H5_SZIP_STATIC_LIBRARY} PARENT_SCOPE)

  set (H5_SZIP_FOUND TRUE PARENT_SCOPE)
endfunction ()

# Main logic for libaec/szip support
if (HDF5_ENABLE_SZIP_SUPPORT)
  set (H5_SZIP_FOUND FALSE)

  # Set package name for libaec/szip if not already set
  if (NOT DEFINED LIBAEC_PACKAGE_NAME)
    set (LIBAEC_PACKAGE_NAME "libaec")
  endif ()

  if (NOT SZIP_USE_EXTERNAL)
    # Find libaec/szip on the system
    system_szip_library ()
  else ()
    # Retrieve libaec/szip from external source (if necessary) and add it to the build process
    # Note that in this case "external" could also mean a local .tgz file on the system
    external_szip_library ()
  endif ()

  if (H5_SZIP_FOUND)
    # Set variables used later on in build process
    set (H5_HAVE_FILTER_SZIP 1)
    set (H5_HAVE_SZLIB_H 1)
    set (H5_HAVE_LIBSZ 1)
    set (HDF5_COMP_INCLUDE_DIRECTORIES "${HDF5_COMP_INCLUDE_DIRECTORIES};${H5_SZIP_INCLUDE_DIRS}")
    if (HDF5_ENABLE_SZIP_ENCODING)
      set (H5_HAVE_SZIP_ENCODER 1)
    endif ()

    # Set variables for libhdf5.settings (and the H5build_settings string)
    if (LIBAEC_PACKAGE_NAME MATCHES "libaec")
      string (TOUPPER "${LIBAEC_PACKAGE_NAME}" LIBAEC_PACKAGE_NAME_UPPER)
      list (APPEND SETTINGS_EXTERNAL_FILTERS "${LIBAEC_PACKAGE_NAME_UPPER}")
    else ()
      list (APPEND SETTINGS_EXTERNAL_FILTERS "SZIP(DECODE)")
      if (HDF5_ENABLE_SZIP_ENCODING)
        list (APPEND SETTINGS_EXTERNAL_FILTERS "SZIP(ENCODE)")
      endif ()
    endif ()

    message (VERBOSE "Filter ${LIBAEC_PACKAGE_NAME} is ON")
  else ()
    if (SZIP_USE_EXTERNAL)
      message (FATAL_ERROR "External support for ${LIBAEC_PACKAGE_NAME} in HDF5 was enabled, but couldn't be processed")
    else ()
      message (FATAL_ERROR "Support for ${LIBAEC_PACKAGE_NAME} in HDF5 was enabled, but ${LIBAEC_PACKAGE_NAME} couldn't be found")
    endif ()
  endif ()
endif ()

# Process list of filters for libhdf5.settings (and the H5build_settings string)
list (REMOVE_DUPLICATES SETTINGS_EXTERNAL_FILTERS)
string (REPLACE ";" " " SETTINGS_EXTERNAL_FILTERS "${SETTINGS_EXTERNAL_FILTERS}")

# Print out compression libraries linked for debugging
message (VERBOSE "LINK_COMP_LIBS=${LINK_COMP_LIBS}")
