# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindCIRCLE
--------

Find the native CIRCLE includes and library

This module defines

::

  CIRCLE_INCLUDE_DIR, where to find CIRCLE.h, etc.
  CIRCLE_LIBRARIES, the libraries required to use CIRCLE.
  CIRCLE_FOUND, If false, do not try to use CIRCLE.

also defined, but not for general use are

::

  CIRCLE_LIBRARY, where to find the CIRCLE library.
#]=======================================================================]

find_path(CIRCLE_INCLUDE_DIR
  NAMES libcircle.h)

find_library(CIRCLE_LIBRARY circle)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(CIRCLE REQUIRED_VARS CIRCLE_LIBRARY CIRCLE_INCLUDE_DIR)

if(CIRCLE_FOUND)
  set(CIRCLE_LIBRARIES ${CIRCLE_LIBRARY} )
endif()

mark_as_advanced(CIRCLE_INCLUDE_DIR CIRCLE_LIBRARY)
