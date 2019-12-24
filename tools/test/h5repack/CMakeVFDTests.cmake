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
        NAME H5REPACK-${vfdname}-h5repacktest-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
              bounds_latest_latest.h5
              h5repack_attr.h5
              h5repack_attr_refs.h5
              h5repack_deflate.h5
              h5repack_early.h5
              h5repack_ext.h5
              h5repack_fill.h5
              h5repack_filters.h5
              h5repack_fletcher.h5
              h5repack_hlink.h5
              h5repack_layout.h5
              h5repack_layouto.h5
              h5repack_layout2.h5
              h5repack_layout3.h5
              h5repack_layout.UD.h5
              h5repack_named_dtypes.h5
              h5repack_nested_8bit_enum.h5
              h5repack_nested_8bit_enum_deflated.h5
              h5repack_nbit.h5
              h5repack_objs.h5
              h5repack_refs.h5
              h5repack_shuffle.h5
              h5repack_soffset.h5
              h5repack_szip.h5
              # fsm
              h5repack_aggr.h5
              h5repack_fsm_aggr_nopersist.h5
              h5repack_fsm_aggr_persist.h5
              h5repack_none.h5
              h5repack_paged_nopersist.h5
              h5repack_paged_persist.h5
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/${vfdname}
    )
    add_test (
        NAME H5REPACK_VFD-${vfdname}-h5repacktest
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5repacktest>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=${vfdname}-h5repacktest.out"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
            -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
    )
    set_tests_properties (H5REPACK_VFD-${vfdname}-h5repacktest PROPERTIES DEPENDS H5REPACK_VFD-${vfdname}-h5repacktest-clear-objects)
    set_tests_properties (H5REPACK_VFD-${vfdname}-h5repacktest PROPERTIES TIMEOUT ${CTEST_SHORT_TIMEOUT})
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Run test with different Virtual File Driver
foreach (vfd ${VFD_LIST})
  ADD_VFD_TEST (${vfd} 0)
endforeach ()
