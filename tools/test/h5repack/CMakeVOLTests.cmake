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

    set (VOL_LIST
    )

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_VOL_TEST volname resultcode)
    add_test (
      NAME H5REPACK-VOL-${volname}-h5repacktest
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:h5repacktest>"
          -D "TEST_ARGS:STRING="
          -D "TEST_VFD:STRING=${volname}"
          -D "TEST_EXPECT=${resultcode}"
          -D "TEST_OUTPUT=h5repacktest"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_DIR}/volTest.cmake"
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5REPACK-VOL-${volname}-h5repacktest PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5REPACK-VOL-${volname}-h5repacktest")
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Run test with different VOL
#  foreach (vol ${VOL_LIST})
#    ADD_VOL_TEST (${vol} 0)
#  endforeach ()
