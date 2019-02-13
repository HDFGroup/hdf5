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

set (SKIP_testphdf5 "")
#if (${HDF5_OPENMPI_VERSION_SKIP})
#  set (SKIP_testphdf5 "${SKIP_testphdf5};-x;ecdsetw")
#endif ()

add_test (NAME MPI_TEST_testphdf5 COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:testphdf5> ${MPIEXEC_POSTFLAGS} ${SKIP_testphdf5})
set_tests_properties (MPI_TEST_testphdf5 PROPERTIES
    FIXTURES_REQUIRED par_clear_testphdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
if (NOT "${last_test}" STREQUAL "")
  set_tests_properties (MPI_TEST_testphdf5 PROPERTIES DEPENDS ${last_test})
endif ()
set (last_test "MPI_TEST_testphdf5")

#if (${HDF5_OPENMPI_VERSION_SKIP})
#  list (REMOVE_ITEM H5P_TESTS t_shapesame)
#endif ()

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

foreach (testp ${H5P_TESTS})
  add_test (NAME MPI_TEST_${testp} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${testp}> ${MPIEXEC_POSTFLAGS})
  set_tests_properties (MPI_TEST_${testp} PROPERTIES
      FIXTURES_REQUIRED par_clear_objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (MPI_TEST_${testp} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "MPI_TEST_${testp}")
endforeach ()

# The t_pflush1 test is hard-coded to fail.
set_tests_properties (MPI_TEST_t_pflush1 PROPERTIES WILL_FAIL "true")
#set_property (TEST MPI_TEST_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
set_tests_properties (MPI_TEST_t_pflush2 PROPERTIES DEPENDS MPI_TEST_t_pflush1)
set_tests_properties (MPI_TEST_t_prestart PROPERTIES DEPENDS MPI_TEST_t_pshutdown)

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
