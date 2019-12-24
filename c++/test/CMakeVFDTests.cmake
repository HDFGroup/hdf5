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

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

set (VFD_LIST
    sec2
    stdio
    core
    split
    multi
    family
)

if (DIRECT_VFD)
  set (VFD_LIST ${VFD_LIST} direct)
endif ()

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

macro (ADD_VFD_TEST vfdname resultcode)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdname}")
    add_test (
        NAME CPP_VFD-${vfdname}-cpp_testhdf5-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            tattr_basic.h5
            tattr_compound.h5
            tattr_dtype.h5
            tattr_multi.h5
            tattr_scalar.h5
            tfattrs.h5
            titerate.h5
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/${vfdname}
    )
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
    set_tests_properties (CPP_VFD-${vfdname}-cpp_testhdf5 PROPERTIES DEPENDS CPP_VFD-${vfdname}-cpp_testhdf5-clear-objects)
    set_tests_properties (CPP_VFD-${vfdname}-cpp_testhdf5 PROPERTIES TIMEOUT ${CTEST_SHORT_TIMEOUT})
  endif ()
endmacro ()

# Run test with different Virtual File Driver
foreach (vfd ${VFD_LIST})
  ADD_VFD_TEST (${vfd} 0)
endforeach ()
