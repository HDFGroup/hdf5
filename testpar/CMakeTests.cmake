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

add_test (NAME TEST_PAR_testphdf5 COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:testphdf5>)

foreach (testp ${H5P_TESTS})
  add_test (NAME TEST_PAR_${testp} COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:${testp}>)
endforeach ()

# The following will only be correct on windows shared
#set_tests_properties (TEST_PAR_t_pflush1 PROPERTIES WILL_FAIL "true")
set_property (TEST TEST_PAR_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
set_tests_properties (TEST_PAR_t_pflush2 PROPERTIES DEPENDS TEST_PAR_t_pflush1)

if (HDF5_TEST_VFD)

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

  macro (ADD_VFD_TEST vfdname resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      foreach (test ${H5P_VFD_TESTS})
        add_test (
          NAME TEST_PAR_VFD-${vfdname}-${test}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${test}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
      endforeach ()
    endif ()
  endmacro ()

  # Run test with different Virtual File Driver
  foreach (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  endforeach ()

endif ()
