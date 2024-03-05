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

set (H5P_VFD_TESTS
    t_pflush1
    t_pflush2
)

set (H5P_VFD_subfiling_TESTS_SKIP
    t_pflush1
    t_pflush2
)

macro (ADD_VFD_TEST vfdname resultcode)
  if (NOT HDF5_USING_ANALYSIS_TOOL)
    foreach (h5_test ${H5P_VFD_TESTS})
      if (NOT "${h5_test}" IN_LIST H5P_VFD_${vfdname}_TESTS_SKIP)
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
        if ("MPI_TEST_VFD-${vfdname}-${h5_test}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (MPI_TEST_VFD-${vfdname}-${h5_test} PROPERTIES DISABLED true)
        endif ()
      endif ()
    endforeach ()
    if (NOT "t_pflush1" IN_LIST H5P_VFD_${vfdname}_TESTS_SKIP)
      set_tests_properties (MPI_TEST_VFD-${vfdname}-t_pflush1 PROPERTIES WILL_FAIL "true")
      #set_property (TEST MPI_TEST_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
    endif ()
    if (NOT "t_pflush2" IN_LIST H5P_VFD_${vfdname}_TESTS_SKIP)
      if (NOT "t_pflush1" IN_LIST H5P_VFD_${vfdname}_TESTS_SKIP)
        set_tests_properties (MPI_TEST_VFD-${vfdname}-t_pflush2 PROPERTIES DEPENDS MPI_TEST_VFD-${vfdname}-t_pflush1)
      endif ()
    endif ()
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
