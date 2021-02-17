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
# Remove any output file left over from previous test run
add_test (
    NAME HL_CPP_ex_ptExampleFL-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
            PTcppexampleFL.h5
            ptExampleFL.txt
)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME HL_CPP_ex_ptExampleFL COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:ptExampleFL>)
else ()
  add_test (NAME HL_CPP_ex_ptExampleFL COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:ptExampleFL>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=ptExampleFL.txt"
      #-D "TEST_REFERENCE=ptExampleFL.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (HL_CPP_ex_ptExampleFL PROPERTIES DEPENDS HL_CPP_ex_ptExampleFL-clear-objects)
