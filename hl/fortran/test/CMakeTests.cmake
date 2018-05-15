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

macro (ADD_H5_FORTRAN_TEST file)
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_FORTRAN_f90_${file} COMMAND $<TARGET_FILE:hl_f90_${file}>)
  else ()
    add_test (NAME HL_FORTRAN_f90_${file} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_${file}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_${file}.txt"
        #-D "TEST_REFERENCE=hl_f90_${file}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_${file} PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)
  if (BUILD_SHARED_LIBS)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME HL_FORTRAN_f90_${file}-shared COMMAND $<TARGET_FILE:hl_f90_${file}-shared>)
    else ()
      add_test (NAME HL_FORTRAN_f90_${file}-shared COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_${file}-shared>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=hl_f90_${file}-shared.txt"
          #-D "TEST_REFERENCE=hl_f90_${file}-shared.out"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (HL_FORTRAN_f90_${file}-shared PROPERTIES DEPENDS HL_FORTRAN_test-shared-clear-objects)
  endif ()
endmacro ()

# Remove any output file left over from previous test run
add_test (
    NAME HL_FORTRAN_test-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        dsetf1.h5
        dsetf2.h5
        dsetf3.h5
        dsetf4.h5
        dsetf5.h5
        f1img.h5
        f1tab.h5
        f2tab.h5
        tstds.h5
)

if (BUILD_SHARED_LIBS)
  add_test (
      NAME HL_FORTRAN_test-shared-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          dsetf1.h5
          dsetf2.h5
          dsetf3.h5
          dsetf4.h5
          dsetf5.h5
          f1img.h5
          f1tab.h5
          f2tab.h5
          tstds.h5
  )
  set_tests_properties (HL_FORTRAN_test-shared-clear-objects
      PROPERTIES DEPENDS "HL_FORTRAN_f90_tsttable;HL_FORTRAN_f90_tstimage;HL_FORTRAN_f90_tstlite;HL_FORTRAN_f90_tstds"
  )
endif ()

foreach (test ${H5_TESTS})
  ADD_H5_FORTRAN_TEST(${test})
endforeach ()
