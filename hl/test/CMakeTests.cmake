
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
MACRO (HL_ADD_TEST hl_name files)
  add_test (NAME HL_${hl_name} COMMAND $<TARGET_FILE:hl_${hl_name}>)
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (HL_${hl_name} PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")

  # --------------------------------------------------------------------
  #-- Copy the necessary files.
  # --------------------------------------------------------------------
  foreach (h5_file ${files})
    set (dest "${HDF5_HL_TEST_BINARY_DIR}/${h5_file}")
    #message (STATUS " Copying HL Test File ${h5_file} to ${dest}")
    add_custom_command (
        TARGET     hl_${hl_name}
        PRE_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_HL_TEST_SOURCE_DIR}/${h5_file} ${dest}
    )

  endforeach (h5_file ${HL_REFERENCE_TEST_FILES})
ENDMACRO (HL_ADD_TEST)

# Remove any output file left over from previous test run
add_test (
    NAME HL_test-clear-objects
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
if (NOT "${last_test}" STREQUAL "")
  set_tests_properties (HL_test-clear-objects PROPERTIES DEPENDS ${last_test})
endif (NOT "${last_test}" STREQUAL "")
set (last_test "HL_test-clear-objects")

HL_add_test (test_ds "dsdata.txt;dslat.txt;dslon.txt;test_ds_be.h5;test_ds_le.h5")
HL_add_test (test_dset_opt "")
HL_add_test (test_image "image8.txt;sepia.pal;earth.pal;image24pixel.txt;image24plane.txt;usa.wri")
HL_add_test (test_lite "dtype_file.txt")
HL_add_test (test_packet "")
HL_add_test (test_table "test_table_be.h5;test_table_cray.h5;test_table_le.h5")
