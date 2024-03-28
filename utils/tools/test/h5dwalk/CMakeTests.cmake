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

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")


##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      message("Entered ADD_H5_TEST - 0")
      add_test (NAME H5DWALK-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dwalk> ${ARGN})
      set_tests_properties (H5DWALK-${resultfile} PROPERTIES
		WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (H5DWALK-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      # Remove any output file left over from previous test run
      add_test (
          NAME H5DWALK-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dwalk>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.h5dwalk"
              -D "TEST_LIBRARY_DIRECTORY=${LL_PATH}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  ADD_H5_TEST(help-1 0 -h)
