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

set (HDF5_TEST_FILES
    image24pixel.txt
    image8.txt
)

foreach (h5_file ${HDF5_TEST_FILES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/${h5_file}" "${PROJECT_BINARY_DIR}/${h5_file}" "hl_ex_ex_ds1_files")
endforeach ()
add_custom_target(hl_ex_ex_ds1_files ALL COMMENT "Copying files needed by hl_ex_ex_ds1 tests" DEPENDS ${hl_ex_ex_ds1_files_list})

# Remove any output file left over from previous test run
add_test (
    NAME HL_ex-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
            ex_lite1.h5
            ex_lite2.h5
            ex_lite3.h5
            packet_table_FLexample.h5
            ex_image1.h5
            ex_image2.h5
            ex_table_01.h5
            ex_table_02.h5
            ex_table_03.h5
            ex_table_04.h5
            ex_table_05.h5
            ex_table_06.h5
            ex_table_07.h5
            ex_table_08.h5
            ex_table_09.h5
            ex_table_10.h5
            ex_table_11.h5
            ex_table_12.h5
            ex_ds1.h5
)
set_tests_properties (HL_ex-clear-objects PROPERTIES FIXTURES_SETUP clear_HL_ex)

foreach (example ${examples})
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_ex_${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:hl_ex_${example}>)
  else ()
    add_test (NAME HL_ex_${example} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_ex_${example}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_ex_${example}.txt"
        #-D "TEST_REFERENCE=hl_ex_${example}.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  if (last_test)
    set_tests_properties (HL_ex_${example} PROPERTIES
        DEPENDS ${last_test}
        FIXTURES_REQUIRED clear_HL_ex
    )
  endif ()
  set (last_test "HL_ex_${example}")
endforeach ()
