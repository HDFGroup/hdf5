#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
# Remove any output file left over from previous test run
add_test (
    NAME MPI_TEST-testphdf5-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ParaTest.h5
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
set_tests_properties (MPI_TEST-testphdf5-clear-objects PROPERTIES FIXTURES_SETUP par_clear_testphdf5)
add_test (
    NAME MPI_TEST-testphdf5-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove ParaTest.h5
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
set_tests_properties (MPI_TEST-testphdf5-clean-objects PROPERTIES FIXTURES_CLEANUP par_clear_testphdf5)

set (SKIP_tests
    cchunk1
    cchunk2
    cchunk3
    cchunk4
    ecdsetw
    eidsetw2
    selnone
    cngrpw-ingrpr
    cschunkw
    ccchunkw
    tldsc
    actualio
    MC_coll_MD_read
)
set (SKIP_testphdf5 "")
foreach (skiptest ${SKIP_tests})
  set (SKIP_testphdf5 "${SKIP_testphdf5};-x;${skiptest}")
endforeach ()

add_test (NAME MPI_TEST_testphdf5 COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:testphdf5> ${MPIEXEC_POSTFLAGS} ${SKIP_testphdf5})
set_tests_properties (MPI_TEST_testphdf5 PROPERTIES
    FIXTURES_REQUIRED par_clear_testphdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
if ("MPI_TEST_testphdf5" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (MPI_TEST_testphdf5 PROPERTIES DISABLED true)
endif ()
if (last_test)
  set_tests_properties (MPI_TEST_testphdf5 PROPERTIES DEPENDS ${last_test})
endif ()
set (last_test "MPI_TEST_testphdf5")

#execute the skipped tests
foreach (skiptest ${SKIP_tests})
  add_test (NAME MPI_TEST_testphdf5_${skiptest} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:testphdf5> ${MPIEXEC_POSTFLAGS} -o ${skiptest})
  set_tests_properties (MPI_TEST_testphdf5_${skiptest} PROPERTIES
      FIXTURES_REQUIRED par_clear_testphdf5
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  if ("MPI_TEST_testphdf5_${skiptest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (MPI_TEST_testphdf5_${skiptest} PROPERTIES DISABLED true)
  endif ()
  if (last_test)
    set_tests_properties (MPI_TEST_testphdf5_${skiptest} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "MPI_TEST_testphdf5_${skiptest}")
endforeach ()

#if (HDF5_OPENMPI_VERSION_SKIP)
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
    test_subfiling_basic_create.h5
    test_subfiling_only_ioc_fail.h5
    test_subfiling_config_file.h5
    test_subfiling_stripe_sizes.h5
    test_subfiling_selection_strategies.h5
    test_subfiling_read_different_stripe_sizes.h5
    test_subfiling_precreate_rank_0.h5
    test_subfiling_write_many_read_one.h5
    test_subfiling_write_many_read_few.h5
    test_subfiling_h5fuse.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME MPI_TEST-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${test_par_CLEANFILES}
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
set_tests_properties (MPI_TEST-clear-objects PROPERTIES FIXTURES_SETUP par_clear_objects)
add_test (
    NAME MPI_TEST-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${test_par_CLEANFILES}
    WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
)
set_tests_properties (MPI_TEST-clean-objects PROPERTIES FIXTURES_CLEANUP par_clear_objects)

foreach (h5_testp ${H5P_TESTS})
  add_test (NAME MPI_TEST_${h5_testp} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${h5_testp}> ${MPIEXEC_POSTFLAGS})
  set_tests_properties (MPI_TEST_${h5_testp} PROPERTIES
      FIXTURES_REQUIRED par_clear_objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_PAR_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  if ("MPI_TEST_${h5_testp}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (MPI_TEST_${h5_testp} PROPERTIES DISABLED true)
  endif ()
  if (last_test)
    set_tests_properties (MPI_TEST_${h5_testp} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "MPI_TEST_${h5_testp}")
endforeach ()

# The t_pflush1 test is hard-coded to fail.
set_tests_properties (MPI_TEST_t_pflush1 PROPERTIES WILL_FAIL "true")
#set_property (TEST MPI_TEST_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
set_tests_properties (MPI_TEST_t_pflush2 PROPERTIES DEPENDS MPI_TEST_t_pflush1)
set_tests_properties (MPI_TEST_t_prestart PROPERTIES DEPENDS MPI_TEST_t_pshutdown)

# h5fuse -p: write a file with several subfiles, combine them with two h5fuse
# ranks, then check the combined file. t_subfiling_vfd only runs h5fuse serially.
# h5fuse is built in utils, which is configured after this file; HDF5_BUILD_UTILS
# defaults to ON.
if (HDF5_ENABLE_SUBFILING_VFD AND (NOT DEFINED HDF5_BUILD_UTILS OR HDF5_BUILD_UTILS))
  add_test (
      NAME MPI_TEST_h5fuse_parallel_mode-clear
      COMMAND ${CMAKE_COMMAND} -E rm -rf h5fuse_parallel_mode
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  set_tests_properties (MPI_TEST_h5fuse_parallel_mode-clear PROPERTIES FIXTURES_SETUP h5fuse_parallel_mode)
  add_test (
      NAME MPI_TEST_h5fuse_parallel_mode-write
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS}
              $<TARGET_FILE:t_h5fuse_parallel_mode> ${MPIEXEC_POSTFLAGS} write
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  # h5fuse -p: each rank combines a share of the subfiles
  add_test (
      NAME MPI_TEST_h5fuse_parallel_mode-fuse
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS}
              ${HDF5_TEST_PAR_BINARY_DIR}/h5fuse ${MPIEXEC_POSTFLAGS} -p -q
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}/h5fuse_parallel_mode
  )
  add_test (
      NAME MPI_TEST_h5fuse_parallel_mode-verify
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS}
              $<TARGET_FILE:t_h5fuse_parallel_mode> ${MPIEXEC_POSTFLAGS} verify
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  add_test (
      NAME MPI_TEST_h5fuse_parallel_mode-clean
      COMMAND ${CMAKE_COMMAND} -E rm -rf h5fuse_parallel_mode
      WORKING_DIRECTORY ${HDF5_TEST_PAR_BINARY_DIR}
  )
  set_tests_properties (MPI_TEST_h5fuse_parallel_mode-clean PROPERTIES FIXTURES_CLEANUP h5fuse_parallel_mode)
  # Chain the steps as fixtures so that selecting any one of them runs the
  # steps it needs
  set_tests_properties (MPI_TEST_h5fuse_parallel_mode-write PROPERTIES
      FIXTURES_REQUIRED h5fuse_parallel_mode
      FIXTURES_SETUP h5fuse_parallel_mode_written
  )
  set_tests_properties (MPI_TEST_h5fuse_parallel_mode-fuse PROPERTIES
      FIXTURES_REQUIRED "h5fuse_parallel_mode;h5fuse_parallel_mode_written"
      FIXTURES_SETUP h5fuse_parallel_mode_fused
  )
  set_tests_properties (MPI_TEST_h5fuse_parallel_mode-verify PROPERTIES
      FIXTURES_REQUIRED "h5fuse_parallel_mode;h5fuse_parallel_mode_fused"
  )
  foreach (h5fuse_test write fuse verify)
    if ("MPI_TEST_h5fuse_parallel_mode-${h5fuse_test}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (MPI_TEST_h5fuse_parallel_mode-${h5fuse_test} PROPERTIES DISABLED true)
    endif ()
  endforeach ()
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
