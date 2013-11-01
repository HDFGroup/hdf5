
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
# Remove any output file left over from previous test run
ADD_TEST (
    NAME cpp_hl_ex_ptExampleFL-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
            PTcppexampleFL.h5
)

ADD_TEST (NAME cpp_hl_ex_ptExampleFL COMMAND $<TARGET_FILE:ptExampleFL>)
SET_TESTS_PROPERTIES (cpp_hl_ex_ptExampleFL PROPERTIES DEPENDS cpp_hl_ex_ptExampleFL-clear-objects)
