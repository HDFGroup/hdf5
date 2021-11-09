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
# FindMFU

# Find the native MFU includes and library

# Imported targets
##################

# This module defines the following :prop_tgt:`IMPORTED` targets:
#
# MFU::MFU
#  The MFU library, if found.
#
# Result variables
###################

# This module will set the following variables in your project:

#  MFU_FOUND, true if the MFU headers and libraries were found.
#  MFU_INCLUDE_DIR, the directory containing the MFU headers.
#  MFU_INCLUDE_DIRS, the directory containing the MFU headers.
#  MFU_LIBRARIES, libraries to link against to use MFU.

# Cache variables
#################

# The following variables may also be set:

#  MFU_LIBRARY, where to find the MFU library.
# message (STATUS "Finding MFU library and headers..." )
#########################################################################


find_path(MFU_INCLUDE_DIR mfu.h)

set(mfu_names ${MFU_NAMES} mfu libmfu)
foreach(name ${mfu_names})
  list (APPEND mfu_names_debug "${name}_debug")
endforeach()

if(NOT MFU_LIBRARY)
  find_library(MFU_LIBRARY NAMES ${mfu_names})
  include(SelectLibraryConfigurations)
  select_library_configurations(MFU)
  mark_as_advanced(MFU_LIBRARY)
endif()
unset(mfu_names)
unset(mfu_names_debug)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(MFU
  REQUIRED_VARS MFU_LIBRARY MFU_INCLUDE_DIR)

if(MFU_FOUND)
  set(MFU_LIBRARIES "${MFU_LIBRARY}")
  set(MFU_LIBRARY_DEBUG "${MFU_LIBRARY}")
  set(MFU_LIBRARY_RELEASE "${MFU_LIBRARY}")
  set(MFU_INCLUDE_DIRS "${MFU_INCLUDE_DIR}")

  if(NOT TARGET MFU::MFU)
    add_library(MFU::MFU UNKNOWN IMPORTED)
    if(MFU_INCLUDE_DIRS)
      set_target_properties(MFU::MFU PROPERTIES
        INTERFACE_INCLUDE_DIRECTORIES "${MFU_INCLUDE_DIRS}")
    endif()
    if(EXISTS "${MFU_LIBRARY}")
      set_target_properties(MFU::MFU PROPERTIES
        IMPORTED_LINK_INTERFACE_LANGUAGES "C"
        IMPORTED_LOCATION "${MFU_LIBRARY}")
    endif()
  endif()
endif()

mark_as_advanced(MFU_LIBRARY MFU_INCLUDE_DIR)

# Report the results.
if (NOT MFU_FOUND)
  set (MFU_DIR_MESSAGE
      "Mfu was not found. Make sure MFU_LIBRARY and MFU_INCLUDE_DIR are set or set the MFU_INSTALL environment variable."
  )
  if (NOT MFU_FIND_QUIETLY)
    if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
      message (VERBOSE "${MFU_DIR_MESSAGE}")
    endif ()
  else ()
    if (MFU_FIND_REQUIRED)
      message (FATAL_ERROR "Mfu was NOT found and is Required by this project")
    endif ()
  endif ()
endif ()
