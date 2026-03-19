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
# HDF5 CMake Filter Plugin Support Configuration
# -----------------------------------------------------------------------------
# This CMake module configures support for external filter plugins in HDF5.
# It provides options for enabling/disabling plugin support, selecting
# static/shared builds, and controlling how plugin dependencies are found or
# built (external, local, or via GIT/TGZ).
#
# Key Features:
# - Options to enable/disable plugin support and select external or local builds.
# - Support for building plugins externally (via GIT or TGZ) or using system libraries.
# - Handles configuration of plugin include directories, library targets, and CMake variables.
# - Sets up required variables for HDF5 to use filter plugins.
#
# Usage:
#   HDF5 includes this file from the main CMakeLists.txt if filter plugin support
#   in HDF5 is enabled (HDF5_ENABLE_PLUGIN_SUPPORT). Configure options as needed before
#   including this file.
#
# See comments throughout for details on each option and logic branch.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# The FetchContent module is needed for building filter plugins from source
# -----------------------------------------------------------------------------
include (FetchContent)

# -----------------------------------------------------------------------------
# Specify major options at the top of the file
# -----------------------------------------------------------------------------
option (PLUGIN_USE_EXTERNAL "Use External Library Building for filter PLUGIN else search" OFF)
cmake_dependent_option (PLUGIN_USE_LOCALCONTENT "Use local file for PLUGIN FetchContent" OFF PLUGIN_USE_EXTERNAL OFF)

# Function to find an HDF5 filter plugins installation on the system with
# find_package()
function (system_hdf5_plugins_library)
  set (HDF5_PLUGINS_FOUND FALSE)

  if (DEFINED PLUGIN_PACKAGE_NAME)
    set (_package_name "${PLUGIN_PACKAGE_NAME}")
  else ()
    set (_package_name "${HDF5_FILTER_PLUGINS_PACKAGE_NAME}")
  endif ()

  find_package (HDF5_PLUGINS NAMES ${_package_name}${HDF_PACKAGE_EXT})

  set (HDF5_PLUGINS_FOUND "${HDF5_PLUGINS_FOUND}" PARENT_SCOPE)
endfunction ()

# Function to retrieve the HDF5 filter plugins project from external source (if
# necessary) and add it to the build process
#
# NOTE: This function does NOT patch the upstream HDF5 filter plugins project and
# will need maintenance for any changes in the CMake target names, installed
# configuration files, etc. in new releases.
function (external_hdf5_plugins_library)
  if (NOT HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT|TGZ")
    message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT must be 'GIT' or 'TGZ' when PLUGIN_USE_EXTERNAL is ON (Current setting: ${HDF5_ALLOW_EXTERNAL_SUPPORT})")
  endif ()

  # Setup for FetchContent
  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
    if (PLUGIN_USE_LOCALCONTENT)
      message (FATAL_ERROR "HDF5_ALLOW_EXTERNAL_SUPPORT '${HDF5_ALLOW_EXTERNAL_SUPPORT}' and PLUGIN_USE_LOCALCONTENT options are mutually exclusive")
    endif ()

    if (DEFINED PLUGIN_GIT_URL)
      # PLUGIN_GIT_URL is deprecated, but still available
      set (_plugins_url "${PLUGIN_GIT_URL}")
    else ()
      set (_plugins_url "${HDF5_FILTER_PLUGINS_GIT_URL}")
    endif ()

    if (DEFINED PLUGIN_GIT_BRANCH)
      # PLUGIN_GIT_BRANCH is deprecated, but still available
      set (_plugins_tag "${PLUGIN_GIT_BRANCH}")
    else ()
      set (_plugins_tag "${HDF5_FILTER_PLUGINS_GIT_TAG}")
    endif ()

    message (STATUS "HDF5 filter plugins project will be built from source ${_plugins_url} (tag ${_plugins_tag})")

    # Instruct FetchContent to retrieve HDF5 filter plugins project from GIT
    FetchContent_Declare (HDF5_FILTER_PLUGINS
        GIT_REPOSITORY ${_plugins_url}
        GIT_TAG ${_plugins_tag}
    )
  else () # HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ"
    if (NOT DEFINED H5PL_TGZPATH)
      set (H5PL_TGZPATH ${TGZPATH})
    endif ()

    if (DEFINED PLUGIN_TGZ_NAME)
      # PLUGIN_TGZ_NAME is deprecated, but still available
      set (_tgz_name "${PLUGIN_TGZ_NAME}")
    else ()
      set (_tgz_name "${HDF5_FILTER_PLUGINS_TGZ_NAME}")
    endif ()

    if (PLUGIN_USE_LOCALCONTENT)
      # Use specified HDF5 filter plugins .tgz file from system
      set (_plugins_url ${H5PL_TGZPATH}/${_tgz_name})
    else ()
      if (DEFINED PLUGIN_TGZ_ORIGPATH)
        # PLUGIN_TGZ_ORIGPATH is deprecated, but still available
        set (_tgz_origpath "${PLUGIN_TGZ_ORIGPATH}")
      else ()
        set (_tgz_origpath "${HDF5_FILTER_PLUGINS_TGZ_ORIGPATH}")
      endif ()

      # Use HDF5 filter plugins .tgz file HDF5_FILTER_PLUGINS_TGZ_NAME / PLUGIN_TGZ_NAME downloaded
      # from base URL HDF5_FILTER_PLUGINS_TGZ_ORIGPATH / PLUGIN_TGZ_ORIGPATH
      set (_plugins_url "${_tgz_origpath}/${_tgz_name}")
    endif ()

    if (PLUGIN_USE_LOCALCONTENT AND NOT EXISTS "${_plugins_url}")
      message (FATAL_ERROR "HDF5 filter plugins file ${_plugins_url} not found (try setting TGZPATH to a directory containing ${_tgz_name})")
    endif ()

    message (STATUS "HDF5 filter plugins project will be built from source ${_plugins_url}")

    # Instruct FetchContent to retrieve HDF5 filter plugins project from .tgz file
    FetchContent_Declare (HDF5_FILTER_PLUGINS
        URL ${_plugins_url}
        URL_HASH ""
    )
  endif ()

  # Include HDF5 filter plugins CMake cache configuration
  include (${HDF_RESOURCES_DIR}/HDF5PluginCache.cmake)

  # Set other options for build. Set CMake policy CMP0077 so that option()
  # commands in Blosc(2) don't override values we set here
  set (CMAKE_POLICY_DEFAULT_CMP0077 NEW)

  if (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT" OR NOT PLUGIN_USE_LOCALCONTENT)
    message (VERBOSE "Fetching and configuring HDF5 filter plugins project")
  else ()
    message (VERBOSE "Configuring HDF5 filter plugins project")
  endif ()

  # If building zlib externally when building HDF5, instruct Blosc(2) to prefer
  # using a system zlib for building so that it doesn't build its own zlib that
  # will conflict with the one HDF5 will build.
  if (HDF5_ENABLE_ZLIB_SUPPORT AND ZLIB_USE_EXTERNAL)
    set (PREFER_EXTERNAL_ZLIB ON)
  endif ()

  # Make HDF5 filter plugins project available for the build
  FetchContent_MakeAvailable (HDF5_FILTER_PLUGINS)

  # Set HDF5 filter plugins directory and status variables
  set (PLUGIN_BINARY_DIR "${hdf5_filter_plugins_BINARY_DIR}" PARENT_SCOPE)

  set (HDF5_PLUGINS_FOUND TRUE PARENT_SCOPE)
endfunction ()

#-----------------------------------------------------------------------------
# Option for HDF5 filter plugins support
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PLUGIN_SUPPORT)
  set (HDF5_PLUGINS_FOUND FALSE)

  if (NOT PLUGIN_USE_EXTERNAL)
    # Find HDF5 filter plugins installation on the system
    system_hdf5_plugins_library ()
  else ()
    # Retrieve HDF5 filter plugins from external source (if necessary) and add
    # them to the build process. Note that in this case "external" could also
    # mean a local .tgz file on the system
    external_hdf5_plugins_library ()
  endif ()

  if (HDF5_PLUGINS_FOUND)
    message (VERBOSE "External HDF5 filter plugins are enabled")
  else ()
    if (PLUGIN_USE_EXTERNAL)
      message (FATAL_ERROR "External support for HDF5 filter plugins was enabled, but couldn't be processed")
    else ()
      message (FATAL_ERROR "Support for filter plugins in HDF5 was enabled, but an HDF5 filter plugins installation couldn't be found")
    endif ()
  endif ()
endif ()
