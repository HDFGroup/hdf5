
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

FOREACH (example ${examples})
  ADD_TEST (NAME hl_ex_${example} COMMAND $<TARGET_FILE:hl_ex_${example}>)
ENDFOREACH (example ${examples})

SET_TESTS_PROPERTIES (hl_ex_ex_lite2 PROPERTIES DEPENDS hl_ex_ex_lite1)
