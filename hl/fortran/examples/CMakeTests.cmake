
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# Remove any output file left over from previous test run
add_test (
    NAME HL_FORTRAN_f90_ex-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ex_ds1.h5
        exlite.h5
)


foreach (example ${examples})
  add_test (NAME HL_FORTRAN_f90_ex_${example} COMMAND $<TARGET_FILE:hl_f90_ex_${example}>)
  set_tests_properties (HL_FORTRAN_f90_ex_${example} PROPERTIES DEPENDS HL_FORTRAN_f90_ex-clear-objects)
endforeach (example ${examples})
