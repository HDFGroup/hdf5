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
file (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)

set (text_dox_ex_CLEANFILES
    d1.h5
)

if (HDF5_TEST_SERIAL)
  # Remove any output file left over from previous test run
  add_test (
      NAME DOXYGEN-EXAMPLES-clear-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${text_dox_ex_CLEANFILES}
  )
  set_tests_properties (DOXYGEN-EXAMPLES-clear-objects PROPERTIES
      FIXTURES_SETUP clear_DOXYGEN_EXAMPLES
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
  add_test (
      NAME DOXYGEN-EXAMPLES-clean-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${text_dox_ex_CLEANFILES}
  )
  set_tests_properties (DOXYGEN-EXAMPLES-clean-objects PROPERTIES
      FIXTURES_CLEANUP clear_DOXYGEN_EXAMPLES
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  foreach (example ${examples})
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME DOXYGEN-EXAMPLES-${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${example}>)
    else ()
      add_test (NAME DOXYGEN-EXAMPLES-${example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${example}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${example}.txt"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (DOXYGEN-EXAMPLES-${example} PROPERTIES FIXTURES_REQUIRED clear_DOXYGEN_EXAMPLES)
    if (last_test)
      set_tests_properties (DOXYGEN-EXAMPLES-${example} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "DOXYGEN-EXAMPLES-${example}")
  endforeach ()
endif ()

### Windows pops up a modal permission dialog on this test
if (H5_HAVE_PARALLEL AND HDF5_TEST_PARALLEL AND NOT WIN32)
  # Ensure that 24 is a multiple of the number of processes.
  # The number 24 corresponds to SPACE1_DIM1 and SPACE1_DIM2 defined in ph5example.c
  math(EXPR NUMPROCS "24 / ((24 + ${MPIEXEC_MAX_NUMPROCS} - 1) / ${MPIEXEC_MAX_NUMPROCS})")

  foreach (parallel_example ${parallel_examples})
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME MPI_TEST_EXAMPLES-${parallel_example} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${parallel_example}> ${MPIEXEC_POSTFLAGS})
    else ()
      add_test (NAME MPI_TEST_EXAMPLES-${parallel_example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=${MPIEXEC_EXECUTABLE};${MPIEXEC_NUMPROC_FLAG};${NUMPROCS};${MPIEXEC_PREFLAGS};$<TARGET_FILE:${parallel_example}>;${MPIEXEC_POSTFLAGS}"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${parallel_example}.out"
          -D "TEST_REFERENCE:STRING=PHDF5 example finished with no errors"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
      )
    endif ()
    if (last_test)
      set_tests_properties (MPI_TEST_EXAMPLES-${parallel_example} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "MPI_TEST_EXAMPLES-${parallel_example}")
  endforeach ()
endif ()
