
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  # Remove any output file left over from previous test run
  add_test (
      NAME CPP_ex-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          Group.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (CPP_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")
  set (last_test "CPP_ex-clear-objects")

  foreach (example ${examples})
    add_test (NAME CPP_ex_${example} COMMAND $<TARGET_FILE:cpp_ex_${example}>)
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (CPP_ex_${example} PROPERTIES DEPENDS ${last_test})
    endif (NOT "${last_test}" STREQUAL "")
    set (last_test "CPP_ex_${example}")
  endforeach (example ${examples})
#the following dependicies are handled by the order of the files
#  SET_TESTS_PROPERTIES(CPP_ex_readdata PROPERTIES DEPENDS CPP_ex_create)
#  SET_TESTS_PROPERTIES(CPP_ex_chunks PROPERTIES DEPENDS CPP_ex_extend_ds)

  add_test (
      NAME CPP_ex_tutr-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          h5tutr_cmprss.h5
          h5tutr_dset.h5
          h5tutr_extend.h5
          h5tutr_group.h5
          h5tutr_groups.h5
          h5tutr_subset.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (CPP_ex_tutr-clear-objects PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")
  set (last_test "CPP_ex_tutr-clear-objects")
  
  foreach (example ${tutr_examples})
    add_test (NAME CPP_ex_${example} COMMAND $<TARGET_FILE:cpp_ex_${example}>)
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (CPP_ex_${example} PROPERTIES DEPENDS ${last_test})
    endif (NOT "${last_test}" STREQUAL "")
    set (last_test "CPP_ex_${example}")
  endforeach (example ${tutr_examples})
#the following dependicies are handled by the order of the files
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_crtatt PROPERTIES DEPENDS CPP_ex_h5tutr_crtdat)
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_rdwt PROPERTIES DEPENDS CPP_ex_h5tutr_crtdat)
#  SET_TESTS_PROPERTIES(CPP_ex_h5tutr_crtgrpd PROPERTIES DEPENDS CPP_ex_h5tutr_crtgrpar)
  