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
add_test (
    NAME HL_FORTRAN_f90_ex-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ex_ds1.h5
        exlite.h5
)


foreach (example ${examples})
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_FORTRAN_f90_ex_${example} COMMAND $<TARGET_FILE:hl_f90_ex_${example}>)
  else ()
    add_test (NAME HL_FORTRAN_f90_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_ex_${example}.txt"
        #-D "TEST_REFERENCE=hl_f90_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_ex_${example} PROPERTIES DEPENDS HL_FORTRAN_f90_ex-clear-objects)
endforeach ()
