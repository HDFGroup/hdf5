
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# Remove any output file left over from previous test run
ADD_TEST (
    NAME hl_fortran_test-clear-objects
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

ADD_TEST (NAME hl_f90_tstds COMMAND $<TARGET_FILE:hl_f90_tstds>)

ADD_TEST (NAME hl_f90_tstlite COMMAND $<TARGET_FILE:hl_f90_tstlite>)

ADD_TEST (NAME hl_f90_tstimage COMMAND $<TARGET_FILE:hl_f90_tstimage>)

ADD_TEST (NAME hl_f90_tsttable COMMAND $<TARGET_FILE:hl_f90_tsttable>)
