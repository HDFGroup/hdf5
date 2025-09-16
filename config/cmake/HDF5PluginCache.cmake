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
# CMake cache file for external HDF5 filter plugins when building the plugins during a HDF5 build

#########################
# EXTERNAL cache entries
#########################

# Enable testing for plugins (examples are the tests for plugins)
set (H5PL_BUILD_TESTING ON CACHE BOOL "Enable H5PL testing" FORCE)
# Enable building of plugin examples if main HDF5 build enables examples
set (H5PL_BUILD_EXAMPLES ${HDF5_BUILD_EXAMPLES} CACHE BOOL "Build H5PL Examples" FORCE)

# Preset HDF5 cache variables to use this project's libraries instead of searching
set (H5PL_HDF5_HEADER "H5pubconf.h" CACHE STRING "Name of HDF5 header" FORCE)
set (H5PL_HDF5_LINK_LIBS ${HDF5_LIBSH_TARGET} CACHE STRING "HDF5 target" FORCE)
# Optionally, could use target property for include dirs (commented out)
#set (H5PL_HDF5_INCLUDE_DIRS $<TARGET_PROPERTY:${HDF5_LIBSH_TARGET},INCLUDE_DIRECTORIES> CACHE PATH "HDF5 include dirs" FORCE)
set (H5PL_HDF5_INCLUDE_DIRS "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR}" CACHE PATH "HDF5 include dirs" FORCE)
set (H5PL_HDF5_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE STRING "HDF5 build folder" FORCE)

# Set up tool executables for plugin tests
set (H5PL_HDF5_DUMP_EXECUTABLE $<TARGET_FILE:h5dump> CACHE STRING "HDF5 h5dump target" FORCE)
set (H5PL_HDF5_REPACK_EXECUTABLE $<TARGET_FILE:h5repack> CACHE STRING "HDF5 h5repack target" FORCE)

# Allow external support for plugins (e.g., GIT or TGZ), defaulting to HDF5 setting if not defined
if (NOT DEFINED H5PL_ALLOW_EXTERNAL_SUPPORT)
  set (H5PL_ALLOW_EXTERNAL_SUPPORT "${HDF5_ALLOW_EXTERNAL_SUPPORT}" CACHE STRING "Allow External Library Building (NO GIT TGZ)" FORCE)
endif ()

# If using GIT for external support, disable certain plugins (BitGroom, JPEG, LZF)
if (H5PL_ALLOW_EXTERNAL_SUPPORT MATCHES "GIT")
  set (ENABLE_BITGROOM OFF CACHE BOOL "" FORCE)
  set (ENABLE_BITROUND OFF CACHE BOOL "" FORCE)
  set (ENABLE_JPEG OFF CACHE BOOL "" FORCE)
  set (ENABLE_LZF OFF CACHE BOOL "" FORCE)
endif ()

# On Windows with Intel compilers, disable ZFP plugin (not supported)
if (WIN32 AND (CMAKE_C_COMPILER_ID MATCHES "Intel[Ll][Ll][Vv][Mm]" OR CMAKE_C_COMPILER_ID MATCHES "Intel"))
  set (ENABLE_ZFP OFF CACHE BOOL "" FORCE)
endif ()

# Set path for plugin TGZ file if not already defined
if (NOT DEFINED H5PL_TGZPATH)
  set (H5PL_TGZPATH "${TGZPATH}" CACHE PATH "PATH for finding plugin tgz file" FORCE)
  mark_as_advanced (H5PL_TGZPATH)
endif ()

# Set GIT and TGZ plugin source variables from parent project
set (H5PL_GIT_URL "${PLUGIN_GIT_URL}" CACHE STRING "Use plugins from HDF Group repository" FORCE)
set (H5PL_GIT_BRANCH "${PLUGIN_GIT_BRANCH}" CACHE STRING "" FORCE)
mark_as_advanced (H5PL_GIT_URL)
mark_as_advanced (H5PL_GIT_BRANCH)
set (H5PL_TGZ_NAME "${PLUGIN_TGZ_NAME}" CACHE STRING "Use plugins from compressed file" FORCE)
mark_as_advanced (H5PL_TGZ_NAME)

# Set plugin package name and CPack option
set (PL_PACKAGE_NAME "${PLUGIN_PACKAGE_NAME}" CACHE STRING "Name of plugins package" FORCE)
mark_as_advanced (PL_PACKAGE_NAME)
set (H5PL_CPACK_ENABLE OFF CACHE BOOL "Enable CPack include and components" FORCE)

# Use GNU install directory variables if enabled in HDF5 build
set (H5PL_USE_GNU_DIRS ${HDF5_USE_GNU_DIRS} CACHE BOOL "ON to use GNU Coding Standard install directory variables, OFF to use historical settings" FORCE)
mark_as_advanced (H5PL_USE_GNU_DIRS)
