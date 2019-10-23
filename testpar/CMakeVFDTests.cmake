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

set (H5P_VFD_TESTS
    t_pflush1
    t_pflush2
)

if (DIRECT_VFD)
  set (VFD_LIST ${VFD_LIST} direct)
endif ()

foreach (vfdtest ${VFD_LIST})
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdtest}")
endforeach ()

macro (ADD_VFD_TEST vfdname resultcode)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    foreach (h5_test ${H5P_VFD_TESTS})
      add_test (
          NAME MPI_TEST_VFD-${vfdname}-${h5_test}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${h5_test}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdname}-${h5_test}.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (MPI_TEST_VFD-${vfdname}-${h5_test} PROPERTIES
          ENVIRONMENT "srcdir=${HDF5_TEST_PAR_BINARY_DIR}/${vfdname}"
          WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}/${vfdname}
      )
    endforeach ()
    set_tests_properties (MPI_TEST_VFD-${vfdname}-pflush1 PROPERTIES WILL_FAIL "true")
    #set_property (TEST MPI_TEST_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
    set_tests_properties (MPI_TEST_VFD-${vfdname}-pflush2 PROPERTIES DEPENDS MPI_TEST_VFD-${vfdname}-pflush1)
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Run test with different Virtual File Driver
foreach (h5_vfd ${VFD_LIST})
  ADD_VFD_TEST (${h5_vfd} 0)
endforeach ()
