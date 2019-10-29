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

set (test_ex_fortran_CLEANFILES
    compound.h5
    copy1.h5
    copy2.h5
    dsetf.h5
    extend.h5
    FORTRAN.h5
    groupf.h5
    groupsf.h5
    h5_cmprss.h5
    mount1.h5
    mount2.h5
    sdsf.h5
    subset.h5
    SDScompound.h5
    test.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME f90_ex-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${test_ex_fortran_CLEANFILES}
)
set_tests_properties (f90_ex-clear-objects PROPERTIES FIXTURES_SETUP clear_f90_ex)

foreach (example ${examples})
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME f90_ex_${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:f90_ex_${example}>)
  else ()
    add_test (NAME f90_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:f90_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=f90_ex_${example}.txt"
        #-D "TEST_REFERENCE=f90_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (f90_ex_${example} PROPERTIES FIXTURES_REQUIRED clear_f90_ex)
  if (last_test)
    set_tests_properties (f90_ex_${example} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "f90_ex_${example}")
endforeach ()

foreach (example ${F2003_examples})
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME f03_ex_${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:f03_ex_${example}>)
  else ()
    add_test (NAME f03_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:f03_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=f03_ex_${example}.txt"
        #-D "TEST_REFERENCE=f03_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (f03_ex_${example} PROPERTIES FIXTURES_REQUIRED clear_f90_ex)
  if (last_test)
    set_tests_properties (f03_ex_${example} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "f03_ex_${example}")
endforeach ()

if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  add_test (NAME MPI_TEST_f90_ex_ph5example COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:f90_ex_ph5example> ${MPIEXEC_POSTFLAGS})
endif ()
