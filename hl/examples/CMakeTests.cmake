
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

SET (HDF5_TEST_FILES
    image24pixel.txt
    image8.txt
)

FOREACH (h5_file ${HDF5_TEST_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
  #MESSAGE (STATUS " Copying ${h5_file}")
  ADD_CUSTOM_COMMAND (
      TARGET     hl_ex_ex_ds1
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${h5_file} ${dest}
  )
ENDFOREACH (h5_file ${HDF5_TEST_FILES})

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME hl_ex-clear-objects
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
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (hl_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "hl_ex-clear-objects")

FOREACH (example ${examples})
  ADD_TEST (NAME hl_ex_${example} COMMAND $<TARGET_FILE:hl_ex_${example}>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (hl_ex_${example} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "hl_ex_${example}")
ENDFOREACH (example ${examples})
