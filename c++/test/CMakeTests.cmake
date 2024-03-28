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
HDFTEST_COPY_FILE("${HDF5_CPP_TEST_SOURCE_DIR}/th5s.h5" "${PROJECT_BINARY_DIR}/th5s.h5" "cpp_testhdf5_files")
add_custom_target(cpp_testhdf5_files ALL COMMENT "Copying files needed by cpp_testhdf5 tests" DEPENDS ${cpp_testhdf5_files_list})

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
if (HDF5_USING_ANALYSIS_TOOL)
  add_test (NAME CPP_testhdf5 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:cpp_testhdf5>)
else ()
  add_test (NAME CPP_testhdf5 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:cpp_testhdf5>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=cpp_testhdf5.txt"
      #-D "TEST_REFERENCE=cpp_testhdf5.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (CPP_testhdf5 PROPERTIES
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
if ("CPP_testhdf5" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (CPP_testhdf5 PROPERTIES DISABLED true)
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
