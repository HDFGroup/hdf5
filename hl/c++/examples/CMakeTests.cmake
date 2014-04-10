
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
# Remove any output file left over from previous test run
add_test (
    NAME HL_CPP_ex_ptExampleFL-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
            PTcppexampleFL.h5
)

add_test (NAME HL_CPP_ex_ptExampleFL COMMAND $<TARGET_FILE:ptExampleFL>)
set_tests_properties (HL_CPP_ex_ptExampleFL PROPERTIES DEPENDS HL_CPP_ex_ptExampleFL-clear-objects)
