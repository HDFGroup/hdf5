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

set (HL_REFERENCE_TEST_FILES
    dtype_file.txt
    image8.txt
    sepia.pal
    earth.pal
    image24pixel.txt
    image24plane.txt
    usa.wri
    test_table_be.h5
    test_table_cray.h5
    test_table_le.h5
    dsdata.txt
    dslat.txt
    dslon.txt
    test_ds_be.h5
    test_ds_le.h5
    test_ld.h5
)

# --------------------------------------------------------------------
#-- Copy the necessary files.
# --------------------------------------------------------------------
foreach (h5_file ${HL_REFERENCE_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_HL_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_HL_TEST_BINARY_DIR}/${h5_file}" "hl_test_files")
endforeach ()
add_custom_target(hl_test_files ALL COMMENT "Copying files needed by hl_test tests" DEPENDS ${hl_test_files_list})

# Remove any output file left over from previous test run
set (test_hl_CLEANFILES
    combine_tables1.h5
    combine_tables2.h5
    file_img1.h5
    file_img2.h5
    test_append.h5
    h5do_compat.h5
    test_detach.h5
    test_ds1.h5
    test_ds2.h5
    test_ds3.h5
    test_ds4.h5
    test_ds5.h5
    test_ds6.h5
    test_ds7.h5
    test_ds8.h5
    test_ds9.h5
    test_ds10.h5
    test_image1.h5
    test_image2.h5
    test_image3.h5
    test_lite1.h5
    test_lite2.h5
    test_lite3.h5
    test_lite4.h5
    test_packet_compress.h5
    test_packet_table.h5
    test_packet_table_vlen.h5
    testfl_packet_table_vlen.h5
    test_table.h5
)
add_test (
    NAME HL_test-clear-objects
    COMMAND    ${CMAKE_COMMAND} -E remove ${test_hl_CLEANFILES}
)
set_tests_properties (HL_test-clear-objects PROPERTIES FIXTURES_SETUP clear_test_hl)

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
macro (HL_ADD_TEST hl_name)
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_${hl_name} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:hl_${hl_name}>)
  else ()
    add_test (NAME HL_${hl_name} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_${hl_name}>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_${hl_name}.txt"
        #-D "TEST_REFERENCE=hl_${hl_name}.out"
        -D "TEST_FOLDER=${HDF5_HL_TEST_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_${hl_name} PROPERTIES
      FIXTURES_REQUIRED clear_test_hl
      ENVIRONMENT "srcdir=${HDF5_HL_TEST_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_HL_TEST_BINARY_DIR}
  )
endmacro ()

HL_add_test (test_lite )
HL_add_test (test_image)
HL_add_test (test_file_image)
HL_add_test (test_table)
HL_add_test (test_ds)
HL_add_test (test_packet)
HL_add_test (test_ld)
HL_add_test (test_dset_append)
HL_add_test (test_h5do_compat)

