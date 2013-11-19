
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
MACRO (HL_ADD_TEST hl_name files)
  ADD_TEST (NAME hl_${hl_name} COMMAND $<TARGET_FILE:hl_${hl_name}>)
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (hl_${hl_name} PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")

  # --------------------------------------------------------------------
  #-- Copy the necessary files.
  # --------------------------------------------------------------------
  FOREACH (h5_file ${files})
    SET (dest "${HDF5_HL_TEST_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying HL Test File ${h5_file} to ${dest}")
    ADD_CUSTOM_COMMAND (
        TARGET     hl_${hl_name}
        PRE_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_HL_TEST_SOURCE_DIR}/${h5_file} ${dest}
    )

  ENDFOREACH (h5_file ${HL_REFERENCE_TEST_FILES})
ENDMACRO (HL_ADD_TEST)

# Remove any output file left over from previous test run
ADD_TEST (
    NAME hl_test-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        combine_tables1.h5
        combine_tables2.h5
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
        test_dectris.h5
        test_image1.h5
        test_image2.h5
        test_image3.h5
        test_lite1.h5
        test_lite2.h5
        test_lite3.h5
        test_lite4.h5
        test_packet_compress.h5
        test_packet_table.h5
        test_table.h5
)
IF (NOT "${last_test}" STREQUAL "")
  SET_TESTS_PROPERTIES (hl_test-clear-objects PROPERTIES DEPENDS ${last_test})
ENDIF (NOT "${last_test}" STREQUAL "")
SET (last_test "hl_test-clear-objects")

HL_ADD_TEST (test_ds "dsdata.txt;dslat.txt;dslon.txt;test_ds_be.h5;test_ds_le.h5")
HL_ADD_TEST (test_dset_opt "")
HL_ADD_TEST (test_image "image8.txt;sepia.pal;earth.pal;image24pixel.txt;image24plane.txt;usa.wri")
HL_ADD_TEST (test_lite "dtype_file.txt")
HL_ADD_TEST (test_packet "")
HL_ADD_TEST (test_table "test_table_be.h5;test_table_cray.h5;test_table_le.h5")
