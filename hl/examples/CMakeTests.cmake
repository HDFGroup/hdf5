
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
  set (dest "${PROJECT_BINARY_DIR}/${h5_file}")
  #message (STATUS " Copying ${h5_file}")
  add_custom_command (
      TARGET     hl_ex_ex_ds1
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${h5_file} ${dest}
  )
endforeach (h5_file ${HDF5_TEST_FILES})

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
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (HL_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")
  set (last_test "HL_ex-clear-objects")

foreach (example ${examples})
  add_test (NAME HL_ex_${example} COMMAND $<TARGET_FILE:hl_ex_${example}>)
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (HL_ex_${example} PROPERTIES DEPENDS ${last_test})
    endif (NOT "${last_test}" STREQUAL "")
    set (last_test "HL_ex_${example}")
endforeach (example ${examples})
