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
#
# This file provides functions for C++ support.
#
#-------------------------------------------------------------------------------
ENABLE_LANGUAGE (CXX)
set (HDF_PREFIX "H5")

#-------------------------------------------------------------------------------
#  Fix CXX flags if we are compiling staticly on Windows using
#  Windows_MT.cmake from config/cmake/UserMacros
#-------------------------------------------------------------------------------
if (BUILD_STATIC_CRT_LIBS)
  TARGET_STATIC_CRT_FLAGS ()
endif ()

#-----------------------------------------------------------------------------
# Configure Checks which require CXX compilation must go in here
# not in the main ConfigureChecks.cmake files, because if the user has
# no CXX compiler, problems arise.
#-----------------------------------------------------------------------------
include (CheckIncludeFileCXX)
include (TestForSTDNamespace)

# For other CXX specific tests, use this MACRO.
macro (HDF_CXX_FUNCTION_TEST OTHER_TEST)
  if (NOT DEFINED ${OTHER_TEST})
    set (MACRO_CHECK_FUNCTION_DEFINITIONS "-D${OTHER_TEST} ${CMAKE_REQUIRED_FLAGS}")
    set (OTHER_TEST_ADD_LIBRARIES)
    if (HDF5_REQUIRED_LIBRARIES)
      set (OTHER_TEST_ADD_LIBRARIES "-DLINK_LIBRARIES:STRING=${HDF5_REQUIRED_LIBRARIES}")
    endif ()

    foreach (def
        HAVE_SYS_TIME_H
        HAVE_UNISTD_H
        HAVE_SYS_TYPES_H
        HAVE_SYS_SOCKET_H
        HAVE_SYS_FILE_H
    )
      if ("${${HDF_PREFIX}_${def}}")
        set (MACRO_CHECK_FUNCTION_DEFINITIONS "${MACRO_CHECK_FUNCTION_DEFINITIONS} -D${def}")
      endif ()
    endforeach ()

    if (LARGEFILE)
      set (MACRO_CHECK_FUNCTION_DEFINITIONS
          "${MACRO_CHECK_FUNCTION_DEFINITIONS} -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE"
      )
    endif ()

    if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
      message (TRACE "Performing ${OTHER_TEST}")
    endif ()
    TRY_COMPILE (${OTHER_TEST}
        ${CMAKE_BINARY_DIR}
        ${HDF_RESOURCES_EXT_DIR}/HDFCXXTests.cpp
        CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${MACRO_CHECK_FUNCTION_DEFINITIONS}
        "${OTHER_TEST_ADD_LIBRARIES}"
        OUTPUT_VARIABLE OUTPUT
    )
    if (${OTHER_TEST} EQUAL 0)
      set (${OTHER_TEST} 1 CACHE INTERNAL "CXX test ${FUNCTION}")
      if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
        message (VERBOSE "Performing CXX Test ${OTHER_TEST} - Success")
      endif ()
    else ()
      if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.15.0")
        message (VERBOSE "Performing CXX Test ${OTHER_TEST} - Failed")
      endif ()
      set (${OTHER_TEST} "" CACHE INTERNAL "CXX test ${FUNCTION}")
      file (APPEND ${CMAKE_BINARY_DIR}/CMakeFiles/CMakeError.log
          "Performing CXX Test ${OTHER_TEST} failed with the following output:\n"
          "${OUTPUT}\n"
      )
    endif ()
  endif ()
endmacro ()

#-----------------------------------------------------------------------------
# Check a bunch of cxx functions
#-----------------------------------------------------------------------------
if (CMAKE_CXX_COMPILER_LOADED)
  foreach (cxx_test
      CXX_HAVE_OFFSETOF
  )
    HDF_CXX_FUNCTION_TEST (${cxx_test})
  endforeach ()
endif ()
