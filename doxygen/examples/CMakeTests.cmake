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
set (text_dox_ex_CLEANFILES
    d1.h5
)

if (HDF5_TEST_SERIAL)
  # Remove any output file left over from previous test run
  add_test (
      NAME DOXYGEN-EXAMPLES-clear-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${text_dox_ex_CLEANFILES}
  )
  set_tests_properties (DOXYGEN-EXAMPLES-clear-objects PROPERTIES
      FIXTURES_SETUP clear_DOXYGEN_EXAMPLES
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
  add_test (
      NAME DOXYGEN-EXAMPLES-clean-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${text_dox_ex_CLEANFILES}
  )
  set_tests_properties (DOXYGEN-EXAMPLES-clean-objects PROPERTIES
      FIXTURES_CLEANUP clear_DOXYGEN_EXAMPLES
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  foreach (example ${examples})
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME DOXYGEN-EXAMPLES-${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${example}>)
    else ()
      add_test (NAME DOXYGEN-EXAMPLES-${example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${example}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${example}.txt"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (DOXYGEN-EXAMPLES-${example} PROPERTIES FIXTURES_REQUIRED clear_DOXYGEN_EXAMPLES)
    set (last_test "DOXYGEN-EXAMPLES-${example}")
  endforeach ()
endif ()
