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
  if (CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (HDF5_CMAKE_C_FLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/general")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/error-general")
  endif ()
  message (VERBOSE "CMAKE_C_FLAGS_GENERAL=${HDF5_CMAKE_C_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable developer warnings
# Developer warnings (suggestions from gcc, not code problems)
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_DEV_WARNINGS)
  message (STATUS "....HDF5 developer group warnings are enabled")
  if (CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/developer-general")
  endif ()

  # Turn on -Winline warnings now only for non-Debug and
  # non-Developer builds. For at least GNU compilers this
  # flag appears to conflict specifically with the -Og
  # optimization flag and will produce warnings about functions
  # not being considered for inlining
  if (NOT ${HDF_CFG_NAME} MATCHES "Debug" AND NOT ${HDF_CFG_NAME} MATCHES "Developer")
    if (CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
      list (APPEND H5_CFLAGS "-Winline")
    endif ()
  endif ()
else ()
  if (CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    ADD_H5_FLAGS (H5_CFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/no-developer-general")
  endif ()
endif ()

