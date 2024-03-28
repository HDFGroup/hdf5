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
H5_CREATE_VFD_DIR()

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################
macro (ADD_VFD_TEST vfdname resultcode)
  if (NOT HDF5_USING_ANALYSIS_TOOL)
    add_test (
        NAME CPP_VFD-${vfdname}-cpp_testhdf5
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:cpp_testhdf5>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=${vfdname}-cpp_testhdf5.out"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
            -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
    )
    set_tests_properties (CPP_VFD-${vfdname}-cpp_testhdf5 PROPERTIES TIMEOUT ${CTEST_SHORT_TIMEOUT})
    if ("CPP_VFD-${vfdname}-cpp_testhdf5" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (CPP_VFD-${vfdname}-cpp_testhdf5 PROPERTIES DISABLED true)
    endif ()
  endif ()
endmacro ()

# Run test with different Virtual File Driver
foreach (vfd ${VFD_LIST})
  ADD_VFD_TEST (${vfd} 0)
endforeach ()
