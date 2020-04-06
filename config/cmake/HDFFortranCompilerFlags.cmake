#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

macro (ADD_H5_FFLAGS h5_fflag_var infile)
  file (STRINGS ${infile} TEST_FLAG_STREAM)
  #message (STATUS "TEST_FLAG_STREAM=${TEST_FLAG_STREAM}")
  list (LENGTH TEST_FLAG_STREAM len_flag)
  if (len_flag GREATER 0)
    math (EXPR _FP_LEN "${len_flag} - 1")
    foreach (line RANGE 0 ${_FP_LEN})
      list (GET TEST_FLAG_STREAM ${line} str_flag)
      string (REGEX REPLACE "^#.*" "" str_flag "${str_flag}")
      #message (STATUS "str_flag=${str_flag}")
      if (str_flag)
        list (APPEND ${h5_fflag_var} "${str_flag}")
      endif ()
    endforeach ()
  endif ()
  #message (STATUS "h5_fflag_var=${${h5_fflag_var}}")
endmacro ()

message (STATUS "Warnings Configuration: default Fortran: ${CMAKE_Fortran_FLAGS}")

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
      set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /warn:none")
    endif ()
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

#-----------------------------------------------------------------------------
# HDF5 library compile options
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CDash is configured to only allow 3000 warnings, so
# break into groups (from the config/gnu-flags file)
#-----------------------------------------------------------------------------
if (NOT MSVC)
  # General flags
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/ifort-general")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-stand:f03" "-free")
  elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-general")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-ffree-form" "-std=f2008" "-fimplicit-none")
  elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "PGI")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "-Mfreeform" "-Mdclchk" "-Mstandard" "-Mallocatable=03")
  endif ()
  message (STATUS "HDF5_CMAKE_Fortran_FLAGS=${HDF5_CMAKE_Fortran_FLAGS}")

  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    # Append warning flags that only gcc 4.4+ knows about
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.4")
  endif ()

  # Append more extra warning flags that only gcc 4.5+ know about
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.5)
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.5")
  endif ()

  # Append more extra warning flags that only gcc 4.6+ know about
  #if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.6)
  #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.6")
  #endif ()

  # Append more extra warning flags that only gcc 4.7+ know about
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.7)
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.7")
  endif ()

  # Append more extra warning flags that only gcc 4.8+ know about
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.8)
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.8")
  endif ()

  # Append more extra warning flags that only gcc 4.9+ know about
  #if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 4.9)
  #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-4.9")
  #endif ()

  # Append more extra warning flags that only gcc 5.1+ know about
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 5.0)
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-5")
  endif ()

  # Append more extra warning flags that only gcc 6.x+ know about
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 6.0)
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-6")
  endif ()

  # Append more extra warning flags that only gcc 7.x+ know about
  #if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 7.0)
  #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-7")
  #endif ()

  # Append more extra warning flags that only gcc 8.x+ know about
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 8.0)
    ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-8")
  endif ()

  # Append more extra warning flags that only gcc 9.x+ know about
  #if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0)
  #  ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/gfort-9")
  #endif ()
else ()
  if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    #ADD_H5_FLAGS (HDF5_CMAKE_Fortran_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/win-ifort-general")
    list (APPEND HDF5_CMAKE_Fortran_FLAGS "/warn:all" "/stand:f90" "/free")
  endif ()
endif ()

