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
# Provides a function to parse version information from H5public.h
# This ensures consistent version extraction across all CMake scripts.
#

#[=======================================================================[.rst:
HDF5VersionParsing
------------------

Provides functions for extracting HDF5 version information from H5public.h

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

# Helper macro for parsing version components
# This macro extracts a version component from the provided content string
# using the specified pattern and sets the result in _parsed_value.
#
# Arguments:
#   content_string - The string to search (should contain version definitions)
#   component_name - The name of the component to find (e.g., "H5_VERS_MAJOR")
#   pattern - The regex pattern to extract the value (with capture group)
#   source_file - The source file path (for error messages)
#
# Sets: _parsed_value in the calling scope
macro(_parse_version_component content_string component_name pattern source_file)
  string(REGEX MATCH "${component_name}[ \t]+${pattern}" _match "${content_string}")
  if(NOT _match)
    message(FATAL_ERROR "Failed to parse ${component_name} from ${source_file}")
  endif()
  set(_parsed_value ${CMAKE_MATCH_1})
endmacro()

function(parse_hdf5_version H5PUBLIC_H_PATH)
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
  file(STRINGS "${H5PUBLIC_H_PATH}" _h5_vers_lines REGEX "^#define H5_VERS_(MAJOR|MINOR|RELEASE|SUBRELEASE)")

  # Convert list to single string with newlines for proper regex matching
  string(REPLACE ";" "\n" _h5_vers_multiline_string "${_h5_vers_lines}")

  # Extract version numbers using helper macro with explicit string passing
  _parse_version_component("${_h5_vers_multiline_string}" "H5_VERS_MAJOR" "([0-9]+)" "${H5PUBLIC_H_PATH}")
  set(${PARSE_VER_MAJOR_VAR} ${_parsed_value} PARENT_SCOPE)

  _parse_version_component("${_h5_vers_multiline_string}" "H5_VERS_MINOR" "([0-9]+)" "${H5PUBLIC_H_PATH}")
  set(${PARSE_VER_MINOR_VAR} ${_parsed_value} PARENT_SCOPE)

  _parse_version_component("${_h5_vers_multiline_string}" "H5_VERS_RELEASE" "([0-9]+)" "${H5PUBLIC_H_PATH}")
  set(${PARSE_VER_RELEASE_VAR} ${_parsed_value} PARENT_SCOPE)

  # Extract subrelease if requested
  if(PARSE_VER_SUBRELEASE_VAR)
    _parse_version_component("${_h5_vers_multiline_string}" "H5_VERS_SUBRELEASE" "\"([^\"]*)\"" "${H5PUBLIC_H_PATH}")
    set(${PARSE_VER_SUBRELEASE_VAR} ${_parsed_value} PARENT_SCOPE)
  endif()

  # Clean up temporary variables
  unset(_h5_vers_lines)
  unset(_h5_vers_multiline_string)
  unset(_match)
  unset(_parsed_value)
endfunction()
