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
set (CMAKE_C_STANDARD 99)
set (CMAKE_C_STANDARD_REQUIRED TRUE)

set (CMAKE_C_FLAGS "${CMAKE_C99_STANDARD_COMPILE_OPTION} ${CMAKE_C_FLAGS}")
set (CMAKE_C_FLAGS "${CMAKE_C_SANITIZER_FLAGS} ${CMAKE_C_FLAGS}")
message (VERBOSE "Warnings Configuration: C default: ${CMAKE_C_FLAGS}")
#-----------------------------------------------------------------------------
# Compiler specific flags
#-----------------------------------------------------------------------------
if (WIN32 AND (CMAKE_C_COMPILER_ID STREQUAL "Intel" OR CMAKE_C_COMPILER_ID MATCHES "IntelLLVM"))
  set (_INTEL_WINDOWS 1)
endif ()

if (WIN32 AND CMAKE_C_COMPILER_ID MATCHES "[Cc]lang" AND "x${CMAKE_C_SIMULATE_ID}" STREQUAL "xMSVC")
  set (_CLANG_MSVC_WINDOWS 1)
endif ()

# Disable deprecation warnings for standard C functions.
# really only needed for newer versions of VS, but should
# not hurt other versions, and this will work into the
# future
if (MSVC OR _INTEL_WINDOWS OR _CLANG_MSVC_WINDOWS)
  add_definitions (-D_CRT_SECURE_NO_DEPRECATE -D_CRT_NONSTDC_NO_DEPRECATE)
endif ()

if (MSVC)
  set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -stack:10000000")
endif ()

# MSVC 14.28 enables C5105, but the Windows SDK 10.0.18362.0 triggers it.
if (CMAKE_C_COMPILER_ID STREQUAL "MSVC" AND NOT CMAKE_C_COMPILER_VERSION VERSION_LESS 19.28)
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -wd5105")
endif ()

if(_CLANG_MSVC_WINDOWS AND "x${CMAKE_C_COMPILER_FRONTEND_VARIANT}" STREQUAL "xGNU")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Xlinker -stack:20000000")
endif()

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
option (HDF5_DISABLE_COMPILER_WARNINGS "Disable compiler warnings" OFF)
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W0")
  endif ()
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif ()
  # Borland uses -w- to suppress warnings.
  if (BORLAND)
    set (HDF5_WARNINGS_BLOCKED 1)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-")
  endif ()

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w")
  endif ()
endif ()

if (CMAKE_C_COMPILER_ID MATCHES "NVHPC" )
  include (${HDF_RESOURCES_DIR}/HDFNvhpcFlags.cmake)
endif ()

if (CMAKE_C_COMPILER_ID MATCHES "GNU")
  include (${HDF_RESOURCES_DIR}/HDFGnuFlags.cmake)
endif ()

if (CMAKE_C_COMPILER_ID MATCHES "Intel")
  include (${HDF_RESOURCES_DIR}/HDFGnuFlags.cmake)
endif ()

if (CMAKE_C_COMPILER_ID MATCHES "Clang")
  include (${HDF_RESOURCES_DIR}/HDFGnuFlags.cmake)
endif ()

if (CMAKE_C_COMPILER_ID MATCHES "MSVC")
  include (${HDF_RESOURCES_DIR}/HDFMsvcFlags.cmake)
endif ()

#-----------------------------------------------------------------------------
# HDF5 library compile options - to be made available to all targets
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Option to allow the user to interpret certain warnings as errors
#
# This should NOT be on by default as it can cause a lot of conflicts with
# new operating systems and compiler versions. Header files that are out of
# our control (MPI, HDFS, etc.) can also raise warnings.
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_WARNINGS_AS_ERRORS "Interpret some warnings as errors" OFF)
if (HDF5_ENABLE_WARNINGS_AS_ERRORS)
  message (STATUS "...some warnings will be interpreted as errors")
endif ()

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
  if (CMAKE_C_COMPILER_ID STREQUAL "PGI")
    list (APPEND HDF5_CMAKE_C_FLAGS "-Minform=inform")
  endif ()
  message (VERBOSE "CMAKE_C_FLAGS_GENERAL=${HDF5_CMAKE_C_FLAGS}")
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable developer warnings
# Developer warnings (suggestions from gcc, not code problems)
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_DEV_WARNINGS "Enable HDF5 developer group warnings" OFF)
if (${HDF_CFG_NAME} MATCHES "Developer")
  # Developer build modes should always have these types of warnings enabled
  set (HDF5_ENABLE_DEV_WARNINGS ON CACHE BOOL "Enable HDF5 developer group warnings" FORCE)
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable debug output
# from various HDF5 modules
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_DEBUG_APIS "Turn on extra debug output in all packages" OFF)
if (HDF5_ENABLE_DEBUG_APIS)
  # Add standard debug definitions to any existing ones
  list (APPEND HDF5_DEBUG_APIS
      H5AC_DEBUG
      H5CX_DEBUG
      H5D_DEBUG
      H5D_CHUNK_DEBUG
      H5F_DEBUG
      H5MM_DEBUG
      H5O_DEBUG
      H5T_DEBUG
      H5Z_DEBUG
  )
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable all warnings
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_ALL_WARNINGS "Enable all warnings" ON)
if (HDF5_ENABLE_ALL_WARNINGS)
  message (STATUS "....All Warnings are enabled")
  if (MSVC)
    if (HDF5_ENABLE_DEV_WARNINGS)
      string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
      list (APPEND HDF5_CMAKE_C_FLAGS "/Wall" "/wd4668")
    else ()
      string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
      list (APPEND HDF5_CMAKE_C_FLAGS "/W3" "/wd4100" "/wd4706" "/wd4127")
    endif ()
  else ()
    list (APPEND HDF5_CMAKE_C_FLAGS ${H5_CFLAGS})
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-asserts
# By default, CMake adds NDEBUG to CMAKE_${lang}_FLAGS for Release build types
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
#option (HDF5_ENABLE_ASSERTS "Determines whether NDEBUG is defined to control assertions." OFF)
set (HDF5_ENABLE_ASSERTS "OFF" CACHE STRING "Determines whether NDEBUG is defined to control assertions (OFF NO YES)")
set_property (CACHE HDF5_ENABLE_ASSERTS PROPERTY STRINGS OFF NO YES)
if (HDF5_ENABLE_ASSERTS MATCHES "YES")
  add_compile_options ("-UNDEBUG")
elseif (HDF5_ENABLE_ASSERTS MATCHES "NO")
  add_compile_options ("-DNDEBUG")
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_ASSERTS)

#-----------------------------------------------------------------------------
# Option for --enable-symbols
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
#option (HDF5_ENABLE_SYMBOLS "Add debug symbols to the library independent of the build mode and optimization level." OFF)
set (HDF5_ENABLE_SYMBOLS "OFF" CACHE STRING "Add debug symbols to the library independent of the build mode and optimization level (OFF NO YES)")
set_property (CACHE HDF5_ENABLE_SYMBOLS PROPERTY STRINGS OFF NO YES)
MARK_AS_ADVANCED (HDF5_ENABLE_SYMBOLS)

#-----------------------------------------------------------------------------
# Option for --enable-profiling
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_PROFILING "Enable profiling flags independently from the build mode." OFF)
if (HDF5_ENABLE_PROFILING)
  list (APPEND HDF5_CMAKE_C_FLAGS "${PROFILE_CFLAGS}")
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_PROFILING)

#-----------------------------------------------------------------------------
# Option for --enable-optimization
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
option (HDF5_ENABLE_OPTIMIZATION "Enable optimization flags/settings independently from the build mode" OFF)
if (HDF5_ENABLE_OPTIMIZATION)
  list (APPEND HDF5_CMAKE_C_FLAGS "${OPTIMIZE_CFLAGS}")
endif ()
MARK_AS_ADVANCED (HDF5_ENABLE_OPTIMIZATION)
