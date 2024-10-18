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
# This file included from HDFCompilerFlags.cmake with
#  if (CMAKE_CXX_COMPILER_ID MATCHES "[Cc]lang")
###############################################################################

#-----------------------------------------------------------------------------
# Compiler specific flags
#-----------------------------------------------------------------------------
if (WIN32 AND "x${CMAKE_CXX_SIMULATE_ID}" STREQUAL "xMSVC")
  set (_CLANG_MSVC_WINDOWS 1)
endif()

#-----------------------------------------------------------------------------
# HDF5 library compile options - to be made available to all targets
#-----------------------------------------------------------------------------

if (NOT ${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
  # General flags
  #
  # Note that some of the flags listed here really should be developer
  # flags (listed in a separate variable, below) but we put them here
  # because they are not raised by the current code and we'd like to
  # know if they do start showing up.
  #
  # NOTE: Don't add -Wpadded here since we can't/won't fix the (many)
  # warnings that are emitted. If you need it, add it at configure time.
  ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/general")
  message (VERBOSE "CMAKE_CXX_FLAGS_GENERAL=${HDF5_CMAKE_CXX_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable developer warnings
# Developer warnings (suggestions from gcc, not code problems)
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_DEV_WARNINGS)
  message (STATUS "....HDF5 CXX developer group warnings are enabled")
  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/developer-general")
else ()
  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/no-developer-general")
endif ()
