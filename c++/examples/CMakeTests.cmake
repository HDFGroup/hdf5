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
      NAME CPP_ex-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          Group.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (CPP_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "CPP_ex-clear-objects")

  foreach (example ${examples})
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME CPP_ex_${example} COMMAND $<TARGET_FILE:cpp_ex_${example}>)
    else ()
      add_test (NAME CPP_ex_${example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:cpp_ex_${example}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=cpp_ex_${example}.txt"
          #-D "TEST_REFERENCE=cpp_ex_${example}.out"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (CPP_ex_${example} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "CPP_ex_${example}")
  endforeach ()
#the following dependencies are handled by the order of the files
#  SET_TESTS_PROPERTIES(CPP_ex_readdata PROPERTIES DEPENDS CPP_ex_create)
#  SET_TESTS_PROPERTIES(CPP_ex_chunks PROPERTIES DEPENDS CPP_ex_extend_ds)

  add_test (
      NAME CPP_ex_tutr-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          h5tutr_cmprss.h5
          h5tutr_dset.h5
          h5tutr_extend.h5
          h5tutr_group.h5
          h5tutr_groups.h5
          h5tutr_subset.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (CPP_ex_tutr-clear-objects PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "CPP_ex_tutr-clear-objects")

  foreach (example ${tutr_examples})
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME CPP_ex_${example} COMMAND $<TARGET_FILE:cpp_ex_${example}>)
    else ()
      add_test (NAME CPP_ex_${example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:cpp_ex_${example}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=cpp_ex_${example}.txt"
          #-D "TEST_REFERENCE=cpp_ex_${example}.out"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (CPP_ex_${example} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "CPP_ex_${example}")
  endforeach ()
#the following dependencies are handled by the order of the files
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_crtatt PROPERTIES DEPENDS CPP_ex_h5tutr_crtdat)
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_rdwt PROPERTIES DEPENDS CPP_ex_h5tutr_crtdat)
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_crtgrpd PROPERTIES DEPENDS CPP_ex_h5tutr_crtgrpar)
