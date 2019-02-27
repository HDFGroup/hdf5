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
#############################################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file        ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")                 ###
####  DEFAULT:                                                                            ###
####         BUILD_SHARED_LIBS:BOOL=OFF                                                   ###
####         HDF_BUILD_C:BOOL=ON                                                          ###
####         HDF_BUILD_CXX:BOOL=OFF                                                       ###
####         HDF_BUILD_FORTRAN:BOOL=OFF                                                   ###
####         HDF_BUILD_JAVA:BOOL=OFF                                                      ###
####         BUILD_TESTING:BOOL=OFF                                                       ###
####         HDF_ENABLE_PARALLEL:BOOL=OFF                                                 ###
####         HDF_ENABLE_THREADSAFE:BOOL=OFF                                               ###
#############################################################################################

### uncomment/comment and change the following lines for other configuration options
### build with shared libraries
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=ON")

#############################################################################################
####      languages       ####
### disable C builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_C:BOOL=OFF")

### enable C++ builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_CXX:BOOL=ON")

### enable Fortran builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_FORTRAN:BOOL=ON")

### enable JAVA builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_JAVA:BOOL=ON")

#############################################################################################
### enable parallel program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_ENABLE_PARALLEL:BOOL=ON")


#############################################################################################
### enable threadsafe program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_ENABLE_THREADSAFE:BOOL=ON")


#############################################################################################
### enable test program builds, requires reference files in testfiles subdirectory
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=ON")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCOMPARE_TESTING:BOOL=ON")

#############################################################################################
#  Do not edit below this line
#############################################################################################
#-----------------------------------------------------------------------------
# MAC machines need special option
#-----------------------------------------------------------------------------
if (APPLE)
  # Compiler choice
  execute_process (COMMAND xcrun --find cc OUTPUT_VARIABLE XCODE_CC OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process (COMMAND xcrun --find c++ OUTPUT_VARIABLE XCODE_CXX OUTPUT_STRIP_TRAILING_WHITESPACE)
  set (ENV{CC} "${XCODE_CC}")
  set (ENV{CXX} "${XCODE_CXX}")
  if (NOT NO_MAC_FORTRAN)
    # Shared fortran is not supported, build static
    set (BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=OFF -DCMAKE_ANSI_CFLAGS:STRING=-fPIC")
  else ()
    set (BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF_BUILD_FORTRAN:BOOL=OFF")
  endif ()
  set (BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCTEST_USE_LAUNCHERS:BOOL=ON -DCMAKE_BUILD_WITH_INSTALL_RPATH:BOOL=OFF")
else ()
  set (BUILD_OPTIONS "${ADD_BUILD_OPTIONS}")
endif ()

#-----------------------------------------------------------------------------
set (CTEST_CMAKE_COMMAND "\"${CMAKE_COMMAND}\"")
## --------------------------
if (CTEST_USE_TAR_SOURCE)
  ## Uncompress source if tar or zip file provided
  ## --------------------------
  if (WIN32)
    message (STATUS "extracting... [${CMAKE_EXECUTABLE_NAME} -E tar -xvf ${CTEST_USE_TAR_SOURCE}.zip]")
    execute_process (COMMAND ${CMAKE_EXECUTABLE_NAME} -E tar -xvf ${CTEST_DASHBOARD_ROOT}\\${CTEST_USE_TAR_SOURCE}.zip RESULT_VARIABLE rv)
  else ()
    message (STATUS "extracting... [${CMAKE_EXECUTABLE_NAME} -E tar -xvf ${CTEST_USE_TAR_SOURCE}.tar]")
    execute_process (COMMAND ${CMAKE_EXECUTABLE_NAME} -E tar -xvf ${CTEST_DASHBOARD_ROOT}/${CTEST_USE_TAR_SOURCE}.tar RESULT_VARIABLE rv)
  endif ()

  if (NOT rv EQUAL 0)
    message (STATUS "extracting... [error-(${rv}) clean up]")
    file (REMOVE_RECURSE "${CTEST_SOURCE_DIRECTORY}")
    message (FATAL_ERROR "error: extract of ${CTEST_SOURCE_NAME} failed")
  endif ()
endif()

#-----------------------------------------------------------------------------
## Clear the build directory
## --------------------------
set (CTEST_START_WITH_EMPTY_BINARY_DIRECTORY TRUE)
if (EXISTS "${CTEST_BINARY_DIRECTORY}" AND IS_DIRECTORY "${CTEST_BINARY_DIRECTORY}")
  ctest_empty_binary_directory (${CTEST_BINARY_DIRECTORY})
else ()
  file (MAKE_DIRECTORY "${CTEST_BINARY_DIRECTORY}")
endif ()

# Use multiple CPU cores to build
include (ProcessorCount)
ProcessorCount (N)
if (NOT N EQUAL 0)
  if (NOT WIN32)
    set (CTEST_BUILD_FLAGS -j${N})
  endif ()
  set (ctest_test_args ${ctest_test_args} PARALLEL_LEVEL ${N})
endif ()
set (CTEST_CONFIGURE_COMMAND
    "${CTEST_CMAKE_COMMAND} -C \"${CTEST_SOURCE_DIRECTORY}/config/cmake/cacheinit.cmake\" -DCMAKE_BUILD_TYPE:STRING=${CTEST_CONFIGURATION_TYPE} ${BUILD_OPTIONS} \"-G${CTEST_CMAKE_GENERATOR}\" \"${CTEST_SOURCE_DIRECTORY}\""
)

#-----------------------------------------------------------------------------
## -- set output to english
set ($ENV{LC_MESSAGES}  "en_EN")

#-----------------------------------------------------------------------------
configure_file (${CTEST_SOURCE_DIRECTORY}/config/cmake/CTestCustom.cmake ${CTEST_BINARY_DIRECTORY}/CTestCustom.cmake)
ctest_read_custom_files ("${CTEST_BINARY_DIRECTORY}")
## NORMAL process
## --------------------------
ctest_start (Experimental)
ctest_configure (BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
if (${res} LESS 0 OR ${res} GREATER 0)
  file (APPEND ${CTEST_SCRIPT_DIRECTORY}/FailedCTest.txt "Failed Configure: ${res}\n")
endif ()
if (LOCAL_SUBMIT)
  ctest_submit (PARTS Configure Notes)
endif ()
ctest_build (BUILD "${CTEST_BINARY_DIRECTORY}" APPEND APPEND RETURN_VALUE res NUMBER_ERRORS errval)
if (${res} LESS 0 OR ${res} GREATER 0 OR ${errval} GREATER 0)
  file(APPEND ${CTEST_SCRIPT_DIRECTORY}/FailedCTest.txt "Failed ${errval} Build: ${res}\n")
endif ()
if (LOCAL_SUBMIT)
  ctest_submit (PARTS Build)
endif ()
ctest_test (BUILD "${CTEST_BINARY_DIRECTORY}" APPEND ${ctest_test_args} RETURN_VALUE res)
if (${res} LESS 0 OR ${res} GREATER 0)
  file(APPEND ${CTEST_SCRIPT_DIRECTORY}/FailedCTest.txt "Failed Tests: ${res}\n")
endif ()
if (LOCAL_SUBMIT)
  ctest_submit (PARTS Test)
endif ()
if (${res} LESS 0 OR ${res} GREATER 0)
  message (FATAL_ERROR "tests FAILED")
endif ()
#-----------------------------------------------------------------------------
##############################################################################################################
message (STATUS "DONE")
