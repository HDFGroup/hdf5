#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5. The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

#
# HDF5VersionParsing.cmake
#
# Provides a macro to parse version information from H5public.h
# This ensures consistent version extraction across all CMake scripts.
#

#[=======================================================================[.rst:
HDF5VersionParsing
------------------

Provides macros for extracting HDF5 version information from H5public.h

parse_hdf5_version
^^^^^^^^^^^^^^^^^^

Parses version constants from H5public.h and sets variables in parent scope.

.. code-block:: cmake

  parse_hdf5_version(<path_to_H5public.h>
                     MAJOR_VAR <major_var_name>
                     MINOR_VAR <minor_var_name>
                     RELEASE_VAR <release_var_name>
                     [SUBRELEASE_VAR <subrelease_var_name>])

Reads the specified H5public.h file and extracts version numbers from the
H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE, and optionally H5_VERS_SUBRELEASE
macros. The extracted values are set in the specified variables in the parent scope.

Arguments:
  - ``<path_to_H5public.h>``: Path to the H5public.h file to parse
  - ``MAJOR_VAR``: Variable name to store H5_VERS_MAJOR value
  - ``MINOR_VAR``: Variable name to store H5_VERS_MINOR value
  - ``RELEASE_VAR``: Variable name to store H5_VERS_RELEASE value
  - ``SUBRELEASE_VAR``: (Optional) Variable name to store H5_VERS_SUBRELEASE value

Example:
  .. code-block:: cmake

    include(HDF5VersionParsing)
    parse_hdf5_version("${CMAKE_SOURCE_DIR}/src/H5public.h"
                       MAJOR_VAR H5_VERS_MAJOR
                       MINOR_VAR H5_VERS_MINOR
                       RELEASE_VAR H5_VERS_RELEASE
                       SUBRELEASE_VAR H5_VERS_SUBRELEASE)
    message(STATUS "HDF5 Version: ${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")

#]=======================================================================]

macro(parse_hdf5_version H5PUBLIC_H_PATH)
  # Parse arguments
  set(options "")
  set(oneValueArgs MAJOR_VAR MINOR_VAR RELEASE_VAR SUBRELEASE_VAR)
  set(multiValueArgs "")
  cmake_parse_arguments(PARSE_VER "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Validate required arguments
  if(NOT PARSE_VER_MAJOR_VAR OR NOT PARSE_VER_MINOR_VAR OR NOT PARSE_VER_RELEASE_VAR)
    message(FATAL_ERROR "parse_hdf5_version requires MAJOR_VAR, MINOR_VAR, and RELEASE_VAR arguments")
  endif()

  # Validate H5public.h exists
  if(NOT EXISTS "${H5PUBLIC_H_PATH}")
    message(FATAL_ERROR "H5public.h not found at: ${H5PUBLIC_H_PATH}")
  endif()

  # Read H5public.h
  file(STRINGS "${H5PUBLIC_H_PATH}" _h5_vers_contents REGEX "^#define H5_VERS_(MAJOR|MINOR|RELEASE|SUBRELEASE)")

  # Extract version numbers using regex
  string(REGEX MATCH "H5_VERS_MAJOR[ \t]+([0-9]+)" _match "${_h5_vers_contents}")
  if(NOT CMAKE_MATCH_1)
    message(FATAL_ERROR "Failed to parse H5_VERS_MAJOR from ${H5PUBLIC_H_PATH}")
  endif()
  set(${PARSE_VER_MAJOR_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)

  string(REGEX MATCH "H5_VERS_MINOR[ \t]+([0-9]+)" _match "${_h5_vers_contents}")
  if(NOT CMAKE_MATCH_1)
    message(FATAL_ERROR "Failed to parse H5_VERS_MINOR from ${H5PUBLIC_H_PATH}")
  endif()
  set(${PARSE_VER_MINOR_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)

  string(REGEX MATCH "H5_VERS_RELEASE[ \t]+([0-9]+)" _match "${_h5_vers_contents}")
  if(NOT CMAKE_MATCH_1)
    message(FATAL_ERROR "Failed to parse H5_VERS_RELEASE from ${H5PUBLIC_H_PATH}")
  endif()
  set(${PARSE_VER_RELEASE_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)

  # Extract subrelease if requested
  if(PARSE_VER_SUBRELEASE_VAR)
    string(REGEX MATCH "H5_VERS_SUBRELEASE[ \t]+\"([^\"]*)\"" _match "${_h5_vers_contents}")
    if(NOT CMAKE_MATCH_1)
      message(FATAL_ERROR "Failed to parse H5_VERS_SUBRELEASE from ${H5PUBLIC_H_PATH}")
    endif()
    set(${PARSE_VER_SUBRELEASE_VAR} ${CMAKE_MATCH_1} PARENT_SCOPE)
  endif()

  # Clean up temporary variables
  unset(_h5_vers_contents)
  unset(_match)
endmacro()
