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
# Remove any output file left over from previous test run
add_test (NAME MPI_TEST-clear-testphdf5-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ParaTest.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_PAR_BINARY_DIR}
)
set_tests_properties (MPI_TEST-clear-testphdf5-objects PROPERTIES FIXTURES_SETUP par_clear_testphdf5)

add_test (NAME MPI_TEST_testphdf5 COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:testphdf5> ${MPIEXEC_POSTFLAGS})
set_tests_properties (MPI_TEST_testphdf5 PROPERTIES
    FIXTURES_REQUIRED par_clear_testphdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
if (last_test)
  set_tests_properties (MPI_TEST_testphdf5 PROPERTIES DEPENDS ${last_test})
endif ()
set (last_test "MPI_TEST_testphdf5")

set (test_par_CLEANFILES
    t_cache_image_00.h5
    t_cache_image_01.h5
    t_cache_image_02.h5
    flush.h5
    noflush.h5
    reloc_t_pread_data_file.h5
    reloc_t_pread_group_0_file.h5
    reloc_t_pread_group_1_file.h5
    shutdown.h5
    after_mpi_fin.h5
    #the following should have been removed by the programs
    bigio_test.h5
    CacheTestDummy.h5
    t_filters_parallel.h5
    MPItest.h5
    ShapeSameTest.h5
)

# Remove any output file left over from previous test run
add_test (NAME MPI_TEST-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ${test_par_CLEANFILES}
    WORKING_DIRECTORY
        ${HDF5_TEST_PAR_BINARY_DIR}
)
set_tests_properties (MPI_TEST-clear-objects PROPERTIES FIXTURES_SETUP par_clear_objects)

foreach (h5_testp ${H5P_TESTS})
  add_test (NAME MPI_TEST_${h5_testp} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${h5_testp}> ${MPIEXEC_POSTFLAGS})
  set_tests_properties (MPI_TEST_${h5_testp} PROPERTIES
      FIXTURES_REQUIRED par_clear_objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  if (last_test)
    set_tests_properties (MPI_TEST_${h5_testp} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "MPI_TEST_${h5_testp}")
endforeach ()

# The following will only be correct on windows shared
#set_tests_properties (MPI_TEST_t_pflush1 PROPERTIES WILL_FAIL "true")
set_property (TEST MPI_TEST_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
set_tests_properties (MPI_TEST_t_pflush2 PROPERTIES DEPENDS MPI_TEST_t_pflush1)

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
