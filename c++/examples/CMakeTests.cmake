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
set (CPP_EX_CLEANFILES
    Group.h5
    SDS.h5
    SDScompound.h5
    SDSextendible.h5
    Select.h5
)
add_test (
    NAME CPP_ex-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${CPP_EX_CLEANFILES}
)
set_tests_properties (CPP_ex-clear-objects PROPERTIES
    FIXTURES_SETUP clear_cppex
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
add_test (
    NAME CPP_ex-clean-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${CPP_EX_CLEANFILES}
)
set_tests_properties (CPP_ex-clean-objects PROPERTIES
    FIXTURES_CLEANUP clear_cppex
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

foreach (example ${examples})
  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME CPP_ex_${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:cpp_ex_${example}>)
  else ()
    add_test (NAME CPP_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:cpp_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=cpp_ex_${example}.txt"
        #-D "TEST_REFERENCE=cpp_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (CPP_ex_${example} PROPERTIES FIXTURES_REQUIRED clear_cppex)
  if (last_test)
    set_tests_properties (CPP_ex_${example} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "CPP_ex_${example}")
endforeach ()
#the following dependencies are handled by the order of the files
#  SET_TESTS_PROPERTIES(CPP_ex_readdata PROPERTIES DEPENDS CPP_ex_create)
#  SET_TESTS_PROPERTIES(CPP_ex_chunks PROPERTIES DEPENDS CPP_ex_extend_ds)
set (CPP_EX_TUTR_CLEANFILES
    h5tutr_cmprss.h5
    h5tutr_dset.h5
    h5tutr_extend.h5
    h5tutr_group.h5
    h5tutr_groups.h5
    h5tutr_subset.h5
)
add_test (
    NAME CPP_ex_tutr-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${CPP_EX_TUTR_CLEANFILES}
)
set_tests_properties (CPP_ex_tutr-clear-objects PROPERTIES
    FIXTURES_SETUP clear_cppex_tutr
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
add_test (
    NAME CPP_ex_tutr-clean-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove ${CPP_EX_TUTR_CLEANFILES}
)
set_tests_properties (CPP_ex_tutr-clean-objects PROPERTIES
    FIXTURES_CLEANUP clear_cppex_tutr
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

foreach (example ${tutr_examples})
  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME CPP_ex_${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:cpp_ex_${example}>)
  else ()
    add_test (NAME CPP_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:cpp_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=cpp_ex_${example}.txt"
        #-D "TEST_REFERENCE=cpp_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (CPP_ex_${example} PROPERTIES FIXTURES_REQUIRED clear_cppex_tutr)
  if (last_test)
    set_tests_properties (CPP_ex_${example} PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "CPP_ex_${example}")
endforeach ()
#the following dependencies are handled by the order of the files
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_crtatt PROPERTIES DEPENDS CPP_ex_h5tutr_crtdat)
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_rdwt PROPERTIES DEPENDS CPP_ex_h5tutr_crtdat)
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_crtgrpd PROPERTIES DEPENDS CPP_ex_h5tutr_crtgrpar)
