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
# HDF5 CMake Libaec Support Configuration
# -----------------------------------------------------------------------------
# This CMake module configures support for libaec in HDF5. It provides options
# for enabling/disabling libaec support, selecting static/shared builds, and
# controlling how libaec is found or built (from system-installed libraries,
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
cmake_dependent_option (HDF5_USE_LIBAEC_STATIC "Find static AEC library" OFF HDF5_ENABLE_SZIP_SUPPORT OFF)
cmake_dependent_option (HDF5_ENABLE_SZIP_ENCODING "Use SZip Encoding" ON HDF5_ENABLE_SZIP_SUPPORT OFF)
cmake_dependent_option (SZIP_USE_EXTERNAL "Use External Library Building for SZIP" OFF HDF5_ENABLE_SZIP_SUPPORT OFF)
mark_as_advanced (SZIP_USE_EXTERNAL)
cmake_dependent_option (LIBAEC_USE_LOCALCONTENT "Use local file for LIBAEC FetchContent" OFF HDF5_ENABLE_SZIP_SUPPORT OFF)
mark_as_advanced (LIBAEC_USE_LOCALCONTENT)

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

# Function to retrieve libaec/szip from external source (if necessary) and add it
# to the build process
#
# NOTE: This function does NOT patch upstream libaec and will need maintenance for
# any changes in the CMake target names, installed configuration files, etc. in new
# releases.
function (external_szip_library)
  if (NOT HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT|TGZ")
    message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT must be 'GIT' or 'TGZ' when SZIP_USE_EXTERNAL is ON (Current setting: ${HDF5_ALLOW_EXTERNAL_SUPPORT})")
  endif ()

  # Setup for FetchContent
  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
    if (LIBAEC_USE_LOCALCONTENT)
      message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT '${HDF5_ALLOW_EXTERNAL_SUPPORT}' and LIBAEC_USE_LOCALCONTENT options are mutually exclusive")
    endif ()

    set (SZIP_URL ${LIBAEC_GIT_URL})

    if (DEFINED LIBAEC_GIT_BRANCH)
      # LIBAEC_GIT_BRANCH is deprecated, but still available
      set (SZIP_TAG ${LIBAEC_GIT_BRANCH})
    else ()
      set (SZIP_TAG ${LIBAEC_GIT_TAG})
    endif ()

    message (STATUS "Filter libaec will be built from source ${SZIP_URL} (tag ${SZIP_TAG})")

    # Instruct FetchContent to retrieve libaec from GIT
    FetchContent_Declare (SZIP
        GIT_REPOSITORY ${SZIP_URL}
        GIT_TAG ${SZIP_TAG}
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
      message (FATAL_ERROR "Filter SZIP file ${SZIP_URL} not found (try setting TGZPATH to a directory containing ${LIBAEC_TGZ_NAME})")
    endif ()

    message (STATUS "Filter libaec will be built from source ${SZIP_URL}")

    # Instruct FetchContent to retrieve libaec from .tgz file
    FetchContent_Declare (SZIP
        URL ${SZIP_URL}
        URL_HASH ""
    )
  endif ()

  # Set libaec shared/static library building based off of preference variable
  if (HDF5_USE_LIBAEC_STATIC)
    set (BUILD_SHARED_LIBS OFF)
    set (BUILD_STATIC_LIBS ON)
  else ()
    set (BUILD_SHARED_LIBS ON)
    set (BUILD_STATIC_LIBS OFF)
  endif ()

  # Set libaec options for build. Include libaec packaging logic and override
  # installation of files into the library's CMake install directory
  set (BUILD_TESTING OFF)
  set (libaec_INSTALL_CMAKEDIR "${HDF5_INSTALL_CMAKE_DIR}" CACHE INTERNAL "")
  set (libaec_INCLUDE_PACKAGING ON CACHE INTERNAL "")

  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT" OR NOT LIBAEC_USE_LOCALCONTENT)
    message (VERBOSE "Fetching and configuring filter libaec")
  else ()
    message (VERBOSE "Configuring filter libaec")
  endif ()

  # Make SZIP (libaec) available for the build
  FetchContent_MakeAvailable (SZIP)

  # Hide libaec-specific items from the GUI by default
  mark_as_advanced (AEC_FUZZING)

  # Set expected target names, based on shared/static preference
  # NOTE: These must be maintained with new releases of upstream libaec in
  # order to avoid having to patch the source when exporting targets.
  if (BUILD_STATIC_LIBS)
    set (h5_aec_target_name aec-static)
    set (h5_aec_objects_target_name aec-static-objects)
    set (h5_sz_target_name sz-static)
    set (h5_sz_objects_target_name sz-static-objects)
    set (h5_aec_export_file_name "libaec_static-targets.cmake")
  else ()
    set (h5_aec_target_name aec-shared)
    set (h5_aec_objects_target_name aec-shared-objects)
    set (h5_sz_target_name sz-shared)
    set (h5_sz_objects_target_name sz-shared-objects)
    set (h5_aec_export_file_name "libaec_shared-targets.cmake")
  endif ()
  set (libaec_targets
    "${h5_aec_target_name}" "${h5_aec_objects_target_name}"
    "${h5_sz_target_name}" "${h5_sz_objects_target_name}"
  )
  foreach (libaec_target ${libaec_targets})
    if (NOT TARGET ${libaec_target})
      message (FATAL_ERROR "Expected target ${libaec_target} is missing from build of external libaec")
    endif ()
  endforeach ()

  # Optionally add namespace aliases for targets
  if (HDF_PACKAGE_NAMESPACE)
    foreach (libaec_target ${libaec_targets})
      if (NOT TARGET ${HDF_PACKAGE_NAMESPACE}${libaec_target})
        get_target_property (_aliased_target ${libaec_target} ALIASED_TARGET)
        if (_aliased_target)
          add_library (${HDF_PACKAGE_NAMESPACE}${libaec_target} ALIAS ${_aliased_target})
        else ()
          add_library (${HDF_PACKAGE_NAMESPACE}${libaec_target} ALIAS ${libaec_target})
        endif ()
        unset (_aliased_target)
      endif ()
    endforeach ()
  endif ()

  set (H5_SZIP_LIBRARY "${HDF_PACKAGE_NAMESPACE}${h5_aec_target_name};${HDF_PACKAGE_NAMESPACE}${h5_sz_target_name}")

  # Set include directories for generated and source headers
  set (H5_SZIP_INCLUDE_DIR_GEN "${szip_BINARY_DIR}" PARENT_SCOPE)
  set (H5_SZIP_INCLUDE_DIR "${szip_SOURCE_DIR}/include" PARENT_SCOPE)
  set (H5_SZIP_INCLUDE_DIRS ${H5_SZIP_INCLUDE_DIR_GEN} ${H5_SZIP_INCLUDE_DIR} PARENT_SCOPE)

  set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${H5_SZIP_LIBRARY} PARENT_SCOPE)

  # If built as a sub-project or if cross-compiling, export all exported
  # targets to the build tree. Append to main targets file but keep
  # "libaec::" namespace from upstream.
  if (HDF5_EXTERNALLY_CONFIGURED OR CMAKE_CROSSCOMPILING)
    export (
      TARGETS ${libaec_targets}
      FILE ${HDF5_PACKAGE}${HDF_PACKAGE_EXT}-targets.cmake
      NAMESPACE libaec::
      APPEND
    )
  endif ()

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
