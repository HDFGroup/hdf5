
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME cpp_ex-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          Group.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (cpp_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "cpp_ex-clear-objects")

  FOREACH (example ${examples})
    ADD_TEST (NAME cpp_ex_${example} COMMAND $<TARGET_FILE:cpp_ex_${example}>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (cpp_ex_${example} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "cpp_ex_${example}")
  ENDFOREACH (example ${examples})
