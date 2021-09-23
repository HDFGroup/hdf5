#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
#########################################################################

# - Derived from the FindTiff.cmake and FindJPEG.cmake that is included with cmake
# FindMERCURY

# Find the native MERCURY includes and library

# Imported targets
##################

# This module defines the following :prop_tgt:`IMPORTED` targets:
#
# MERCURY::MERCURY
#  The MERCURY library, if found.
#
# Result variables
###################

# This module will set the following variables in your project:

#  MERCURY_FOUND, true if the MERCURY headers and libraries were found.
#  MERCURY_INCLUDE_DIR, the directory containing the MERCURY headers.
#  MERCURY_INCLUDE_DIRS, the directory containing the MERCURY headers.
#  MERCURY_LIBRARIES, libraries to link against to use MERCURY.

# Cache variables
#################

# The following variables may also be set:

#  MERCURY_LIBRARY, where to find the MERCURY library.
#  MERCURY_LIBRARY_DEBUG - Debug version of MERCURY library
#  MERCURY_LIBRARY_RELEASE - Release Version of MERCURY library

# message (STATUS "Finding MERCURY library and headers..." )
#########################################################################


find_path(MERCURY_INCLUDE_DIR mercury.h)

set(mercury_names ${MERCURY_NAMES} mercury libmercury_util libmercury)
foreach(name ${mercury_names})
  list (APPEND mercury_names_debug "${name}_debug")
endforeach()

if(NOT MERCURY_LIBRARY)
  find_library(MERCURY_LIBRARY_RELEASE NAMES ${mercury_names})
  find_library(MERCURY_LIBRARY_DEBUG NAMES ${mercury_names_debug})
  include(SelectLibraryConfigurations)
  select_library_configurations(MERCURY)
  mark_as_advanced(MERCURY_LIBRARY_RELEASE MERCURY_LIBRARY_DEBUG)
endif()
unset(mercury_names)
unset(mercury_names_debug)

if(MERCURY_INCLUDE_DIR AND EXISTS "${MERCURY_INCLUDE_DIR}/mercury_config.h")
    file(STRINGS "${MERCURY_INCLUDE_DIR}/mercury_proc.h" mercury_version_str
         REGEX "^#define[\t ]+HG_VERSION[\t ]+.*")

    string(REGEX REPLACE "^#define[\t ]+MERCURY_PACKAGE_VERSION[\t ]+([0-9]+).*"
           "\\1" HG_VERSION "${mercury_version_str}")
    unset(mercury_version_str)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MERCURY
  REQUIRED_VARS MERCURY_LIBRARY MERCURY_INCLUDE_DIR
  VERSION_VAR MERCURY_VERSION)

if(MERCURY_FOUND)
  set(MERCURY_LIBRARIES ${MERCURY_LIBRARY})
  set(MERCURY_INCLUDE_DIRS "${MERCURY_INCLUDE_DIR}")

  if(NOT TARGET MERCURY::MERCURY)
    add_library(MERCURY::MERCURY UNKNOWN IMPORTED)
    if(MERCURY_INCLUDE_DIRS)
      set_target_properties(MERCURY::MERCURY PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${MERCURY_INCLUDE_DIRS}")
    endif()
    if(EXISTS "${MERCURY_LIBRARY}")
      set_target_properties(MERCURY::MERCURY PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES "C"
        IMPORTED_LOCATION "${MERCURY_LIBRARY}")
    endif()
    if(EXISTS "${MERCURY_LIBRARY_RELEASE}")
      set_property(TARGET MERCURY::MERCURY APPEND PROPERTY
        IMPORTED_CONFIGURATIONS RELEASE)
      set_target_properties(MERCURY::MERCURY PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "C"
        IMPORTED_LOCATION_RELEASE "${MERCURY_LIBRARY_RELEASE}")
    endif()
    if(EXISTS "${MERCURY_LIBRARY_DEBUG}")
      set_property(TARGET MERCURY::MERCURY APPEND PROPERTY
        IMPORTED_CONFIGURATIONS DEBUG)
      set_target_properties(MERCURY::MERCURY PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES_DEBUG "C"
        IMPORTED_LOCATION_DEBUG "${MERCURY_LIBRARY_DEBUG}")
    endif()
  endif()
endif()

mark_as_advanced(MERCURY_LIBRARY MERCURY_INCLUDE_DIR)

# Report the results.
if (NOT MERCURY_FOUND)
  set (MERCURY_DIR_MESSAGE
      "Mercury was not found. Make sure MERCURY_LIBRARY and MERCURY_INCLUDE_DIR are set or set the MERCURY_INSTALL environment variable."
  )
  if (NOT MERCURY_FIND_QUIETLY)
    if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
      message (VERBOSE "${MERCURY_DIR_MESSAGE}")
    endif ()
  else ()
    if (MERCURY_FIND_REQUIRED)
      message (FATAL_ERROR "Mercury was NOT found and is Required by this project")
    endif ()
  endif ()
endif ()
