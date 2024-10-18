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

message (VERBOSE "Warnings Configuration: default Fortran: ${CMAKE_Fortran_FLAGS}")

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
  endif ()
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif ()
  # Borland uses -w- to suppress warnings.
  if (BORLAND)
    set (HDF5_WARNINGS_BLOCKED 1)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w-")
  endif ()

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w")
  endif ()
endif ()

if (CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  include (${HDF_RESOURCES_DIR}/HDFNvhpcFortranFlags.cmake)
endif ()

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  include (${HDF_RESOURCES_DIR}/HDFGnuFortranFlags.cmake)
endif ()

if (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  include (${HDF_RESOURCES_DIR}/HDFIntelFortranFlags.cmake)
endif ()

#-----------------------------------------------------------------------------
# HDF5 library compile options - to be made available to all targets
#-----------------------------------------------------------------------------
if (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
    message (STATUS "... Select IEEE floating-point mode full")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-ieee=full")
endif ()

if (NOT MSVC AND NOT MINGW)
  # General flags
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-Mfreeform" "-Mdclchk" "-Mstandard" "-Mallocatable=03")
  endif ()
  message (VERBOSE "HDF5_CMAKE_Fortran_FLAGS=${HDF5_CMAKE_Fortran_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# The build mode flags are not added to CMAKE_Fortran_FLAGS, so create a separate
# variable for them so they can be written out to libhdf5.settings and
# H5build_settings.c
#-----------------------------------------------------------------------------
if ("${HDF_CFG_NAME}" STREQUAL       "Debug")
  set (HDF5_BUILD_MODE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEBUG}")
elseif ("${HDF_CFG_NAME}" STREQUAL   "Developer")
  set (HDF5_BUILD_MODE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_DEVELOPER}")
elseif ("${HDF_CFG_NAME}" STREQUAL   "Release")
  set (HDF5_BUILD_MODE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_RELEASE}")
elseif ("${HDF_CFG_NAME}" STREQUAL   "MinSizeRel")
  set (HDF5_BUILD_MODE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_MINSIZEREL}")
elseif ("${HDF_CFG_NAME}" STREQUAL   "RelWithDebInfo")
  set (HDF5_BUILD_MODE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS_RELWITHDEBINFO}")
else ()
  set (HDF5_BUILD_MODE_Fortran_FLAGS "")
endif ()

