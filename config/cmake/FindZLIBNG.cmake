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
#########################################################################

# CMake module to find the zlib-ng library on the system, preferring to
# make use of a CMake configuration file for the library if one is found.
# Derived from the FindZLIB.cmake file that is included with CMake.
#
# Imported Targets
# ^^^^^^^^^^^^^^^^
#
# If a CMake configuration file cannot be located, this module defines
# the following `IMPORTED` targets:
#
# ``zlib-ng::zlib``
#   The zlib-ng library, if found and only if zlib-ng was NOT built with
#   zlib compatibility.
#
# ``zlib-ng::zlibstatic``
#   The static zlib-ng library, if found and only if zlib-ng was NOT built
#   with zlib compatibility.
#
# ``ZLIB::ZLIB``
#   The zlib-ng library, if found and only if zlib-ng was built with zlib
#   compatibility.
#
# ``ZLIB::zlibstatic``
#   The static zlib-ng library, if found and only if zlib-ng was built with
#   zlib compatibility.
#
# Result Variables
# ^^^^^^^^^^^^^^^^
#
# This module defines the following variables:
#
# ``ZLIBNG_FOUND``
#   "True" if ``zlib-ng`` is found.
#
# ``ZLIBNG_INCLUDE_DIRS``
#   The zlib-ng include directories.
#
# ``ZLIBNG_LIBRARIES``
#   The libraries to link against when using zlib-ng.
#
# ``ZLIBNG_VERSION``
#   The version of zlib-ng found. May not be set if a version number
#   can't be parsed from zlib-ng.h/zlib.h or a pkg-config file.
#
# ``ZLIB_VERSION``
#   The version of zlib found. May not be set if a version number
#   can't be parsed from zlib-ng.h/zlib.h or a pkg-config file. Will
#   only be set if ZLIBNG_ZLIB_COMPAT is TRUE.
#
# ``ZLIBNG_ZLIB_COMPAT``
#   "True" if zlib-ng was built with zlib compatibility.
#
# Hints
# ^^^^^
#
# Set ``ZLIBNG_ROOT`` to a directory which contains a zlib-ng installation.
#

include (FindPackageHandleStandardArgs)

set (ZLIBNG_VALID_COMPONENTS
  shared
  static
)

set (ZLIBNG_ZLIB_COMPAT FALSE)

macro (get_zlibng_version header)
  file (STRINGS "${header}" zlibng_version_str REGEX "^#define[\t ]+ZLIBNG_VERSION[\t ]+\".*\"")
endmacro ()

# First, check to see if zlib-ng was built with CMake and has a zlib-ng-config.cmake
# file available for use.
if (DEFINED ZLIBNG_ROOT AND NOT ZLIBNG_FIND_QUIETLY)
  message (VERBOSE "Looking for zlib-ng CMake configuration file at ${ZLIBNG_ROOT}")
endif ()
find_package (ZLIBNG QUIET CONFIG NAMES "zlib-ng")
mark_as_advanced (ZLIBNG_DIR)

# If zlib-ng wasn't found, try finding zlib in case zlib-ng was built with zlib
# compatibility
if (NOT ZLIBNG_FOUND)
  set (ZLIBNG_FOUND FALSE)
  unset (ZLIBNG_DIR)

  find_package (ZLIBNG QUIET CONFIG NAMES "zlib")
  if (ZLIBNG_FOUND)
    # Searching by name "zlib" should be enough to only locate zlib-ng's zlib,
    # but check the header file just in case
    if (DEFINED ZLIB_INCLUDE_DIR AND EXISTS "${ZLIB_INCLUDE_DIR}/zlib.h")
      # Accept zlib.h only if it defines ZLIBNG_VERSION so we don't pick
      # up a regular zlib installation for zlib-ng
      get_zlibng_version ("${ZLIB_INCLUDE_DIR}/zlib.h")
      if (zlibng_version_str)
        set (ZLIBNG_ZLIB_COMPAT TRUE)
      else ()
        if (NOT ZLIBNG_FIND_QUIETLY)
          message (VERBOSE "Rejecting zlib.h file that doesn't define ZLIBNG_VERSION")
        endif ()

        set (ZLIBNG_FOUND FALSE)
      endif ()
      unset (get_zlibng_version)
    else ()
      set (ZLIBNG_FOUND FALSE)
    endif ()
  endif ()
endif ()

# If a zlib-ng-config.cmake (or zlib-config.cmake) file is available for use, prefer that
if (ZLIBNG_FOUND)
  unset (ZLIBNG_shared_FOUND)
  unset (ZLIBNG_static_FOUND)

  # Set variables that this module returns
  if (NOT ZLIBNG_ZLIB_COMPAT)
    if (NOT TARGET zlib-ng::zlib AND NOT TARGET zlib-ng::zlibstatic)
      set (ZLIBNG_FOUND FALSE)
      set (ZLIBNG_NOT_FOUND_MESSAGE "zlib-ng was found but CMake targets (zlib-ng::zlib, zlib-ng::zlibstatic) were missing")
      return ()
    endif ()

    if (TARGET zlib-ng::zlib)
      set (ZLIBNG_shared_FOUND TRUE)
      if (NOT ZLIBNG_USE_STATIC_LIBS)
        set (ZLIBNG_LIBRARIES "zlib-ng::zlib")
      endif ()
    endif ()
    if (TARGET zlib-ng::zlibstatic)
      set (ZLIBNG_static_FOUND TRUE)
      if (ZLIBNG_USE_STATIC_LIBS)
        set (ZLIBNG_LIBRARIES "zlib-ng::zlibstatic")
      endif ()
    endif ()
  else ()
    if (NOT TARGET ZLIB::ZLIB AND NOT TARGET ZLIB::zlibstatic)
      set (ZLIBNG_FOUND FALSE)
      set (ZLIBNG_NOT_FOUND_MESSAGE "zlib-ng was found but CMake targets (ZLIB::ZLIB, ZLIB::zlibstatic) were missing")
      return ()
    endif ()

    if (TARGET ZLIB::ZLIB)
      set (ZLIBNG_shared_FOUND TRUE)
      if (NOT ZLIBNG_USE_STATIC_LIBS)
        set (ZLIBNG_LIBRARIES "ZLIB::ZLIB")
      endif ()
    endif ()
    if (TARGET ZLIB::zlibstatic)
      set (ZLIBNG_static_FOUND TRUE)
      if (ZLIBNG_USE_STATIC_LIBS)
        set (ZLIBNG_LIBRARIES "ZLIB::zlibstatic")
      endif ()
    endif ()
  endif ()

  # Determine zlib-ng include directory (or directories)
  foreach (zlibng_target ${ZLIBNG_LIBRARIES})
    if (NOT TARGET ${zlibng_target})
      continue ()
    endif ()

    get_target_property (zlibng_include_dir_prop ${zlibng_target} INTERFACE_INCLUDE_DIRECTORIES)
    if (zlibng_include_dir_prop)
      list (APPEND ZLIBNG_INCLUDE_DIRS "${zlibng_include_dir_prop}")
    endif ()
  endforeach ()
  list (REMOVE_DUPLICATES ZLIBNG_INCLUDE_DIRS)
  unset (zlibng_include_dir_prop)

  find_package_handle_standard_args (ZLIBNG
    REQUIRED_VARS ZLIBNG_LIBRARIES ZLIBNG_INCLUDE_DIRS
    VERSION_VAR ZLIBNG_VERSION
    HANDLE_COMPONENTS
    CONFIG_MODE
  )
  if (ZLIBNG_FOUND AND ZLIBNG_DIR AND NOT ZLIBNG_FIND_QUIETLY)
    message (VERBOSE "Found existing zlib-ng CMake configuration file at ${ZLIBNG_DIR}")
  endif ()

  return ()
elseif (DEFINED ZLIBNG_NOT_FOUND_MESSAGE AND NOT ZLIBNG_FIND_QUIETLY)
  message (VERBOSE "Couldn't load zlib-ng from CMake configuration file: ${ZLIBNG_NOT_FOUND_MESSAGE}")
endif ()

# Try to find a zlib-ng pkg-config file
find_package (PkgConfig QUIET)
if (PKG_CONFIG_FOUND)
  pkg_check_modules (PC_ZLIBNG QUIET zlib-ng)
  # If zlib-ng wasn't found, try finding zlib in case zlib-ng was built
  # with zlib compatibility
  if (NOT PC_ZLIBNG_FOUND)
    # Check for zlib.pc from zlib-ng before populating scope with variables
    # from the module
    pkg_get_variable (zlib_pkg_includedir zlib "includedir")
    if (zlib_pkg_includedir AND EXISTS "${zlib_pkg_includedir}/zlib.h")
      # Accept zlib.h only if it defines ZLIBNG_VERSION so we don't pick
      # up a regular zlib installation for zlib-ng
      get_zlibng_version ("${zlib_pkg_includedir}/zlib.h")
      if (zlibng_version_str)
        pkg_check_modules (PC_ZLIBNG QUIET zlib)
      endif ()
      unset (zlibng_version_str)
    endif ()
  endif ()
endif ()

# Find the zlib-ng.h header file
find_path (ZLIBNG_INCLUDE_DIR
  NAMES zlib-ng.h
  HINTS ${ZLIBNG_ROOT} ${PC_ZLIBNG_INCLUDE_DIRS}
  DOC "Path to the zlib-ng.h header file"
)
mark_as_advanced (ZLIBNG_INCLUDE_DIR)

# If not found, try to find the zlib.h header file
if (NOT ZLIBNG_INCLUDE_DIR)
  if (NOT ZLIBNG_FIND_QUIETLY)
    message (VERBOSE "Couldn't find zlib-ng.h, searching for zlib.h")
  endif ()

  find_path (ZLIBNG_INCLUDE_DIR
    NAMES zlib.h
    HINTS ${ZLIBNG_ROOT} ${PC_ZLIBNG_INCLUDE_DIRS}
    DOC "Path to the zlib.h header file"
  )
endif ()

if (ZLIBNG_INCLUDE_DIR AND EXISTS "${ZLIBNG_INCLUDE_DIR}/zlib.h")
  # Accept zlib.h only if it defines ZLIBNG_VERSION so we don't pick
  # up a regular zlib installation for zlib-ng
  get_zlibng_version ("${ZLIBNG_INCLUDE_DIR}/zlib.h")
  if (NOT zlibng_version_str)
    if (NOT ZLIBNG_FIND_QUIETLY)
      message (VERBOSE "Rejecting zlib.h file that doesn't define ZLIBNG_VERSION")
    endif ()
    return ()
  else ()
    set (ZLIBNG_ZLIB_COMPAT TRUE)
  endif ()
  unset (zlibng_version_str)
endif ()

# Find the appropriate library based on zlib compatibility
if (NOT ZLIBNG_LIBRARY)
  if (ZLIBNG_USE_STATIC_LIBS)
    unset (_zlibng_find_lib_suffixes_orig)
    if (DEFINED CMAKE_FIND_LIBRARY_SUFFIXES)
      set (_zlibng_find_lib_suffixes_orig "${CMAKE_FIND_LIBRARY_SUFFIXES}")
    endif ()
    if (WIN32)
      set (CMAKE_FIND_LIBRARY_SUFFIXES .lib .a ${CMAKE_FIND_LIBRARY_SUFFIXES})
    else ()
      set(CMAKE_FIND_LIBRARY_SUFFIXES .a)
    endif ()
  endif ()

  if (NOT ZLIBNG_ZLIB_COMPAT)
    if (ZLIBNG_USE_STATIC_LIBS)
      set (_zlibng_lib_names zlibstatic-ng zlib-ng z-ng)
    else ()
      set (_zlibng_lib_names z-ng zlib-ng zlibstatic-ng)
    endif ()

    # Find the zlib-ng library
    find_library (ZLIBNG_LIBRARY
      NAMES ${_zlibng_lib_names}
      HINTS ${ZLIBNG_ROOT} ${PC_ZLIBNG_LIBRARY_DIRS}
      PATH_SUFFIXES lib lib64
      DOC "Path to the zlib-ng library file"
    )
  else ()
    if (ZLIBNG_USE_STATIC_LIBS)
      set (_zlibng_lib_names zlibstatic zlib z)
    else ()
      set (_zlibng_lib_names z zlib zlibstatic)
    endif ()

    # Find the zlib library
    find_library (ZLIBNG_LIBRARY
      NAMES ${_zlibng_lib_names}
      HINTS ${ZLIBNG_ROOT} ${PC_ZLIBNG_LIBRARY_DIRS}
      PATH_SUFFIXES lib lib64
      DOC "Path to the zlib library file"
    )
  endif ()
  mark_as_advanced (ZLIBNG_LIBRARY)
  unset (_zlibng_lib_names)

  if (DEFINED _zlibng_find_lib_suffixes_orig)
    set (CMAKE_FIND_LIBRARY_SUFFIXES "${_zlibng_find_lib_suffixes_orig}")
  else ()
    set (CMAKE_FIND_LIBRARY_SUFFIXES)
  endif ()
endif ()

# Determine zlib-ng version from zlib(-ng).h or from pkg-config file
if (ZLIBNG_INCLUDE_DIR)
  if (NOT ZLIBNG_ZLIB_COMPAT)
    set (_zlibng_header_file "zlib-ng.h")
  else ()
    set (_zlibng_header_file "zlib.h")
  endif ()

  if (EXISTS "${ZLIBNG_INCLUDE_DIR}/${_zlibng_header_file}")
    get_zlibng_version ("${ZLIBNG_INCLUDE_DIR}/${_zlibng_header_file}")
    if (zlibng_version_str)
      string (REGEX REPLACE "^#define[\t ]+ZLIBNG_VERSION[\t ]+\"([^\"]*)\".*" "\\1" ZLIBNG_VERSION "${zlibng_version_str}")
    endif ()
    unset (zlibng_version_str)
  elseif (PC_ZLIBNG_FOUND)
    set (ZLIBNG_VERSION ${PC_ZLIBNG_VERSION})
  endif ()
  # If applicable, determine zlib version from zlib.h or from pkg-config file
  if (ZLIBNG_ZLIB_COMPAT AND EXISTS "${ZLIBNG_INCLUDE_DIR}/zlib.h")
    file (STRINGS "${ZLIBNG_INCLUDE_DIR}/zlib.h" zlib_version_str REGEX "^#define[\t ]+ZLIB_VERSION[\t ]+\".*\"")
    if (zlib_version_str)
      string (REGEX REPLACE "^#define[\t ]+ZLIB_VERSION[\t ]+\"([^\"]*)\".*" "\\1" ZLIB_VERSION "${zlib_version_str}")
    endif ()
    unset (zlib_version_str)
  elseif (PC_ZLIBNG_FOUND)
    set (ZLIB_VERSION ${PC_ZLIBNG_VERSION})
  endif ()

  unset (_zlibng_header_file)
endif ()

if (ZLIBNG_LIBRARY)
  if (ZLIBNG_USE_STATIC_LIBS)
    set (ZLIBNG_static_FOUND TRUE)
  else ()
    set (ZLIBNG_shared_FOUND TRUE)
  endif ()
endif ()

# Set variables for whether zlib-ng was found
find_package_handle_standard_args (ZLIBNG
  REQUIRED_VARS ZLIBNG_LIBRARY ZLIBNG_INCLUDE_DIR
  VERSION_VAR ZLIBNG_VERSION
  HANDLE_COMPONENTS
)

# Created imported targets and set remaining variables for module
if (ZLIBNG_FOUND)
  unset (ZLIBNG_shared_FOUND)
  unset (ZLIBNG_static_FOUND)

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

  if (NOT TARGET ${_zlibng_target_name})
    add_library (${_zlibng_target_name} UNKNOWN IMPORTED)

    set_target_properties(${_zlibng_target_name} PROPERTIES
      IMPORTED_LOCATION "${ZLIBNG_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES "${ZLIBNG_INCLUDE_DIR}"
    )
  endif ()
  unset (_zlibng_target_name)

  set (ZLIBNG_INCLUDE_DIRS ${ZLIBNG_INCLUDE_DIR})
  set (ZLIBNG_LIBRARIES ${ZLIBNG_LIBRARY})
endif ()
