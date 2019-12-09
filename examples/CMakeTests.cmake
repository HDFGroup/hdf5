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
file (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)

set (test_ex_CLEANFILES
    Attributes.h5
    btrees_file.h5
    cmprss.h5
    default_file.h5
    dset.h5
    extend.h5
    extlink_prefix_source.h5
    extlink_source.h5
    extlink_target.h5
    group.h5
    groups.h5
    hard_link.h5
    mount1.h5
    mount2.h5
    one_index_file.h5
    only_dspaces_and_attrs_file.h5
    only_huge_mesgs_file.h5
    REF_REG.h5
    refere.h5
    SDS.h5
    SDScompound.h5
    SDSextendible.h5
    Select.h5
    separate_indexes_file.h5
    small_lists_file.h5
    soft_link.h5
    subset.h5
    unix2win.h5
    blue/prefix_target.h5
    red/prefix_target.h5
    u2w/u2w_target.h5
    vds.h5
    vds-exc.h5
    vds-excalibur.h5
    vds-exclim.h5
    vds-percival.h5
    vds-percival-unlim.h5
    vds-percival-unlim-maxmin.h5
    a.h5
    b.h5
    c.h5
    d.h5
    vds-simpleIO.h5
    vds-eiger.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME EXAMPLES-clear-objects
    COMMAND    ${CMAKE_COMMAND} -E remove ${test_ex_CLEANFILES}
)
set_tests_properties (EXAMPLES-clear-objects PROPERTIES FIXTURES_SETUP clear_EXAMPLES)

foreach (example ${examples})
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME EXAMPLES-${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${example}>)
  else ()
    add_test (NAME EXAMPLES-${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=${example}.txt"
        #-D "TEST_REFERENCE=${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (EXAMPLES-${example} PROPERTIES FIXTURES_REQUIRED clear_EXAMPLES)
  if (last_test)
    set_tests_properties (EXAMPLES-${example} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "EXAMPLES-${example}")
endforeach ()

### Windows pops up a modal permission dialog on this test
if (H5_HAVE_PARALLEL AND NOT WIN32)
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME MPI_TEST_EXAMPLES-ph5example COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:ph5example> ${MPIEXEC_POSTFLAGS})
  else ()
    add_test (NAME MPI_TEST_EXAMPLES-ph5example COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=${MPIEXEC_EXECUTABLE};${MPIEXEC_NUMPROC_FLAG};${MPIEXEC_MAX_NUMPROCS};${MPIEXEC_PREFLAGS};$<TARGET_FILE:ph5example>;${MPIEXEC_POSTFLAGS}"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_OUTPUT=ph5example.out"
        -D "TEST_REFERENCE:STRING=PHDF5 tests finished with no errors"
        -D "TEST_FILTER:STRING=PHDF5 tests finished with no errors"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/grepTest.cmake"
    )
  endif ()
  if (last_test)
    set_tests_properties (MPI_TEST_EXAMPLES-ph5example PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "MPI_TEST_EXAMPLES-ph5example")
endif ()
