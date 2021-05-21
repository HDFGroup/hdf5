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
set(CMAKE_CXX_STANDARD 98)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_CXX_EXTENSIONS OFF)

set (CMAKE_CXX_FLAGS "${CMAKE_CXX_SANITIZER_FLAGS} ${CMAKE_CXX_FLAGS}")
if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
  message (VERBOSE "Warnings Configuration: CXX default:  ${CMAKE_CXX_FLAGS}")
endif ()
#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_LOADED)
  set (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS}")
  if (${HDF_CFG_NAME} MATCHES "Debug")
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Og -ftrapv -fno-common")
    endif ()
  else ()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fstdarg-opt")
    endif ()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" AND NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 10.0)
      if (HDF5_ENABLE_BUILD_DIAGS)
        message (STATUS "... default color and URL extended diagnostic messages enabled")
      else ()
        message (STATUS "... disable color and URL extended diagnostic messages")
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-urls=never -fno-diagnostics-color")
      endif ()
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
if (HDF5_DISABLE_COMPILER_WARNINGS)
  message (STATUS "....Compiler warnings are suppressed")
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  if (MSVC)
    set (HDF5_WARNINGS_BLOCKED 1)
    if (CMAKE_CXX_COMPILER_LOADED)
      string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W0")
    endif ()
  endif ()
  if (WIN32)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
  endif ()

  # Most compilers use -w to suppress warnings.
  if (NOT HDF5_WARNINGS_BLOCKED)
    if (CMAKE_CXX_COMPILER_LOADED)
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# HDF5 library compile options
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# CDash is configured to only allow 3000 warnings, so
# break into groups (from the config/gnu-flags file)
#-----------------------------------------------------------------------------
if (NOT MSVC AND NOT MINGW)
  if (${CMAKE_SYSTEM_NAME} MATCHES "SunOS")
    list (APPEND HDF5_CMAKE_CXX_FLAGS "-erroff=%none -DBSD_COMP")
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
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/intel-warnings/general")
      if(NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 18.0)
        list (APPEND H5_CXXFLAGS "-Wextra-tokens -Wformat -Wformat-security -Wic-pointer -Wshadow")
        list (APPEND H5_CXXFLAGS "-Wsign-compare -Wtrigraphs -Wwrite-strings")
      endif()
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      if (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_LOADED
          AND CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
        # add the general CXX flags for g++ compiler versions 4.8 and above.
        ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-general")
        if (HDF5_ENABLE_WARNINGS_AS_ERRORS)
          ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-error-general")
        else ()
          ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-noerror-general")
        endif ()
      endif ()
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
      ADD_H5_FLAGS (HDF5_CMAKE_CXX_FLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/general")
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "PGI")
      list (APPEND HDF5_CMAKE_CXX_FLAGS "-Minform=inform")
    endif ()
    if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
      message (VERBOSE "CMAKE_CXX_FLAGS_GENERAL=${HDF5_CMAKE_CXX_FLAGS}")
    endif ()
  endif ()

  #-----------------------------------------------------------------------------
  # Option to allow the user to enable developer warnings
  # Developer warnings (suggestions from gcc, not code problems)
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_DEV_WARNINGS)
    message (STATUS "....HDF5 developer group warnings are enabled")
  #  if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  #    list (APPEND H5_CXXFLAGS "-Winline -Wreorder -Wport -Wstrict-aliasing")
  #  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-general")
  #  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  #    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/developer-general")
    endif ()
  else ()
    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-general")
  #  elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  #    ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/clang-warnings/no-developer-general")
    endif ()
  endif ()

  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    # Technically, variable-length arrays are part of the C99 standard, but
    #   we should approach them a bit cautiously... Only needed for gcc 4.X
    if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0 AND CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL 4.8)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.8-4.last")
    endif ()

    # Append more extra warning flags that only gcc 4.8+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.8)
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.8")
      if (HDF5_ENABLE_DEV_WARNINGS)
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-4.8")
      else ()
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-4.8")
      endif ()
    endif ()

    # Append more extra warning flags that only gcc 4.9+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 4.9)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/4.9")
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-4.9")
    endif ()

    # Append more extra warning flags that only gcc 5.1+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 5.0)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-5")
      if (HDF5_ENABLE_WARNINGS_AS_ERRORS)
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-error-5")
      else ()
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/cxx-noerror-5")
      endif ()
    endif ()

    # Append more extra warning flags that only gcc 6.x+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 6.0)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/6")
    endif ()

    # Append more extra warning flags that only gcc 7.x+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 7.0)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXxFLAGS2 "${HDF5_SOURCE_DIR}/config/gnu-warnings/7")
      if (HDF5_ENABLE_DEV_WARNINGS)
        # autotools always add the C flags with the CXX flags
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-7")
      #else ()
      #  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-7")
      endif ()
    endif ()

    # Append more extra warning flags that only gcc 8.x+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 8.0)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/8")
      #if (HDF5_ENABLE_WARNINGS_AS_ERRORS)
      #  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/error-8")
      #else ()
      #  ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/noerror-8")
      #endif ()
      if (HDF5_ENABLE_DEV_WARNINGS)
        # autotools always add the C flags with the CXX flags
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/developer-8")
      else ()
        # autotools always add the C flags with the CXX flags
        ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/no-developer-8")
      endif ()
    endif ()

    # Append more extra warning flags that only gcc 9.x+ know about
    if (NOT CMAKE_CXX_COMPILER_VERSION VERSION_LESS 9.0)
      # autotools always add the C flags with the CXX flags
      ADD_H5_FLAGS (H5_CXXFLAGS "${HDF5_SOURCE_DIR}/config/gnu-warnings/9")
    endif ()
  endif ()
else ()
  if (NOT MINGW)
    list (APPEND HDF5_CMAKE_CXX_FLAGS "/EHsc")
    endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to allow the user to enable all warnings
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_ALL_WARNINGS)
  message (STATUS "....All Warnings are enabled")
  if (MSVC)
    if (HDF5_ENABLE_DEV_WARNINGS)
      if (CMAKE_CXX_COMPILER_LOADED)
        string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        list (APPEND HDF5_CMAKE_CXX_FLAGS "/Wall" "/wd4668")
      endif ()
    else ()
      if (CMAKE_CXX_COMPILER_LOADED)
        string (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
        list (APPEND HDF5_CMAKE_CXX_FLAGS "/W3" "/wd4100" "/wd4706" "/wd4127")
      endif ()
    endif ()
  else ()
    if (CMAKE_CXX_COMPILER_LOADED)
      list (APPEND HDF5_CMAKE_CXX_FLAGS ${H5_CXXFLAGS})
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
if (CMAKE_COMPILER_IS_GNUCXX AND CMAKE_CXX_COMPILER_LOADED)
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0")
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-symbols
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_SYMBOLS MATCHES "YES")
  if(CMAKE_CXX_COMPILER_LOADED)
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    endif ()
  endif ()
elseif (HDF5_ENABLE_SYMBOLS MATCHES "NO")
  if(CMAKE_CXX_COMPILER_LOADED)
    if (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
      set (CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} -Wl,-s")
    elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
      set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -s")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-profiling
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PROFILING)
  if(CMAKE_CXX_COMPILER_LOADED)
    list (APPEND HDF5_CMAKE_CXX_FLAGS "${PROFILE_CXXFLAGS}")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option for --enable-optimization
# This option will force/override the default setting for all configurations
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_OPTIMIZATION)
  if(CMAKE_CXX_COMPILER_LOADED)
    list (APPEND HDF5_CMAKE_CXX_FLAGS "${OPTIMIZE_CXXFLAGS}")
  endif ()
endif ()
