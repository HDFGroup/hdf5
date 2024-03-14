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

# Remove any output file left over from previous test run
set (HL_FORTRAN_F90_EX_CLEANFILES
    ex_ds1.h5
    exlite.h5
)
add_test (
    NAME HL_FORTRAN_f90_ex-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${HL_FORTRAN_F90_EX_CLEANFILES}
)
set_tests_properties (HL_FORTRAN_f90_ex-clear-objects PROPERTIES
    FIXTURES_SETUP clear_HL_FORTRAN_f90_ex
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
add_test (
    NAME HL_FORTRAN_f90_ex-clean-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${HL_FORTRAN_F90_EX_CLEANFILES}
)
set_tests_properties (HL_FORTRAN_f90_ex-clean-objects PROPERTIES
    FIXTURES_CLEANUP clear_HL_FORTRAN_f90_ex
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

foreach (example ${examples})
  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME HL_FORTRAN_f90_ex_${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:hl_f90_ex_${example}>)
  else ()
    add_test (NAME HL_FORTRAN_f90_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_ex_${example}.txt"
        #-D "TEST_REFERENCE=hl_f90_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_ex_${example} PROPERTIES
      FIXTURES_REQUIRED clear_HL_FORTRAN_f90_ex
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
endforeach ()
