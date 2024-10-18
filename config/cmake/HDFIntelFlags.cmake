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

###############################################################################
# This file included from HDFCompilerFlags.cmake
###############################################################################

if (${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
  list (APPEND HDF5_CMAKE_C_FLAGS "-erroff=%none -DBSD_COMP")
else ()
  # General flags
  #
  # Note that some of the flags listed here really should be developer
  # flags (listed in a separate variable, below) but we put them here
  # because they are not raised by the current code and we'd like to
  # know if they do start showing up.
  #
  # NOTE: Don't add -Wpadded here since we can't/won't fix the (many)
  # warnings that are emitted. If you need it, add it at configure time.
  if (CMAKE_C_COMPILER_ID STREQUAL "Intel")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/win-general")
    else ()
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/general")
    endif()
    if (NOT _INTEL_WINDOWS)
      if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 15.0)
        ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/15")
      endif ()
      # this is just a failsafe
      list (APPEND H5_CFLAGS "-finline-functions")
      if (NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 18.0)
        ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/18")
      endif ()
    endif ()
  elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/win-general")
    else ()
      # this is just a failsafe
      list (APPEND H5_CFLAGS "-finline-functions")
      ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/general")
    endif ()
  message (VERBOSE "CMAKE_C_FLAGS_GENERAL=${HDF5_CMAKE_C_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable developer warnings
# Developer warnings (suggestions from gcc, not code problems)
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_DEV_WARNINGS)
  message (STATUS "....HDF5 developer group warnings are enabled")
  if (CMAKE_C_COMPILER_ID STREQUAL "Intel")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/win-developer-general")
    else ()
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/classic/developer-general")
    endif ()
  elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM")
    if (_INTEL_WINDOWS)
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/win-developer-general")
    else ()
      ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/oneapi/developer-general")
    endif ()
  endif ()

  # Turn on -Winline warnings now only for non-Debug and
  # non-Developer builds. For at least GNU compilers this
  # flag appears to conflict specifically with the -Og
  # optimization flag and will produce warnings about functions
  # not being considered for inlining
  if (NOT ${HDF_CFG_NAME} MATCHES "Debug" AND NOT ${HDF_CFG_NAME} MATCHES "Developer")
    if (CMAKE_C_COMPILER_ID STREQUAL "Intel" AND NOT _INTEL_WINDOWS)
      list (APPEND H5_CFLAGS "-Winline")
    elseif (CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" AND NOT _INTEL_WINDOWS)
      list (APPEND H5_CFLAGS "-Winline")
    endif ()
  endif ()
endif ()
