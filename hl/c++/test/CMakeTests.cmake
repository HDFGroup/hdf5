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
set (HL_CPP_PT_CLEANFILES
            packettest.h5
)
add_test (
    NAME HL_CPP_ptableTest-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${HL_CPP_PT_CLEANFILES}
)
set_tests_properties (HL_CPP_ptableTest-clear-objects PROPERTIES
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

if (HDF5_USING_ANALYSIS_TOOL)
  add_test (NAME HL_CPP_ptableTest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:hl_ptableTest>)
else ()
  add_test (NAME HL_CPP_ptableTest COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:hl_ptableTest>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=hl_ptableTest.txt"
      #-D "TEST_REFERENCE=hl_ptableTest.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (HL_CPP_ptableTest PROPERTIES DEPENDS HL_CPP_ptableTest-clear-objects)
if ("HL_CPP_ptableTest" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (HL_CPP_ptableTest PROPERTIES DISABLED true)
endif ()
add_test (
    NAME HL_CPP_ptableTest-clean-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${HL_CPP_PT_CLEANFILES}
)
set_tests_properties (HL_CPP_ptableTest-clean-objects PROPERTIES
    DEPENDS HL_CPP_ptableTest
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

