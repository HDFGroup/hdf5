
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# Remove any output file left over from previous test run
add_test (
    NAME HL_FORTRAN_test-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        dsetf1.h5
        dsetf2.h5
        dsetf3.h5
        dsetf4.h5
        dsetf5.h5
        f1img.h5
        f1tab.h5
        tstds.h5
)

add_test (NAME HL_FORTRAN_f90_tstds COMMAND $<TARGET_FILE:hl_f90_tstds>)
set_tests_properties (HL_FORTRAN_f90_tstds PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

add_test (NAME HL_FORTRAN_f90_tstlite COMMAND $<TARGET_FILE:hl_f90_tstlite>)
set_tests_properties (HL_FORTRAN_f90_tstlite PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

add_test (NAME HL_FORTRAN_f90_tstimage COMMAND $<TARGET_FILE:hl_f90_tstimage>)
set_tests_properties (HL_FORTRAN_f90_tstimage PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

add_test (NAME HL_FORTRAN_f90_tsttable COMMAND $<TARGET_FILE:hl_f90_tsttable>)
set_tests_properties (HL_FORTRAN_f90_tsttable PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)
