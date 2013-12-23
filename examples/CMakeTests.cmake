
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  FILE (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME EXAMPLES-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          Attributes.h5
          btrees_file.h5
          cmprss.h5
          default_file.h5
          dset.h5
          extend.h5 
          extlink_prefix_source.h5
          extlink_source.h5
          extlink_target.h5
          group.h5
          groups.h5
          hard_link.h5
          mount1.h5
          mount2.h5
          one_index_file.h5
          only_dspaces_and_attrs_file.h5
          only_huge_mesgs_file.h5
          REF_REG.h5
          refere.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
          separate_indexes_file.h5
          small_lists_file.h5
          soft_link.h5
          subset.h5
          unix2win.h5
          blue/prefix_target.h5
          red/prefix_target.h5
          u2w/u2w_target.h5
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (EXAMPLES-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "EXAMPLES-clear-objects")

  FOREACH (example ${examples})
    ADD_TEST (NAME EXAMPLES-${example} COMMAND $<TARGET_FILE:${example}>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (EXAMPLES-${example} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "EXAMPLES-${example}")
  ENDFOREACH (example ${examples})

  IF (H5_HAVE_PARALLEL)
    ADD_TEST (NAME EXAMPLES-ph5example COMMAND $<TARGET_FILE:ph5example>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (EXAMPLES-ph5example PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "EXAMPLES-ph5example")
  ENDIF (H5_HAVE_PARALLEL)
