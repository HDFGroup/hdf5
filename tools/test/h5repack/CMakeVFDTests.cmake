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
    add_test (
      NAME H5REPACK-VFD-${vfdname}-h5repacktest
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:h5repacktest>"
          -D "TEST_ARGS:STRING="
          -D "TEST_VFD:STRING=${vfdname}"
          -D "TEST_EXPECT=${resultcode}"
          -D "TEST_OUTPUT=h5repacktest"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5REPACK-VFD-${vfdname}-h5repacktest PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5REPACK-VFD-${vfdname}-h5repacktest")
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
