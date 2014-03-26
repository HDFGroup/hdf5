
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
# Remove any output file left over from previous test run
add_test (
    NAME cpp_hl_ex_ptExampleFL-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
            PTcppexampleFL.h5
)

add_test (NAME cpp_hl_ex_ptExampleFL COMMAND $<TARGET_FILE:ptExampleFL>)
set_tests_properties (cpp_hl_ex_ptExampleFL PROPERTIES DEPENDS cpp_hl_ex_ptExampleFL-clear-objects)
