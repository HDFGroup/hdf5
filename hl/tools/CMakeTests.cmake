
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# Make testfiles dir under build dir
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

#
# copy test files from source to build dir
#

add_custom_command (
    TARGET     gif2h5
    POST_BUILD
    COMMAND    ${CMAKE_COMMAND}
    ARGS       -E copy_if_different ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/testfiles/image1.gif ${PROJECT_BINARY_DIR}/testfiles/image1.gif
)

add_custom_command (
    TARGET     h52gif
    POST_BUILD
    COMMAND    ${CMAKE_COMMAND}
    ARGS       -E copy_if_different ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/testfiles/h52giftst.h5 ${PROJECT_BINARY_DIR}/testfiles/h52giftst.h5
)

add_custom_command (
    TARGET     h52gif
    POST_BUILD
    COMMAND    ${CMAKE_COMMAND}
    ARGS       -E copy_if_different ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/testfiles/ex_image2.h5 ${PROJECT_BINARY_DIR}/testfiles/ex_image2.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME HL_TOOLS-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        image1.gif
        image1.h5
        image.gif
        image24.gif
)

add_test (NAME HL_TOOLS_gif2h5 COMMAND $<TARGET_FILE:gif2h5> testfiles/image1.gif image1.h5)

add_test (NAME HL_TOOLS_h52gif COMMAND $<TARGET_FILE:h52gif> testfiles/h52giftst.h5 image1.gif -i image)

add_test (NAME HL_TOOLS_h52gif_none COMMAND $<TARGET_FILE:h52gif> testfiles/h52giftst.h5 image.gif -i nosuch_image)
set_tests_properties (HL_TOOLS_h52gif_none PROPERTIES WILL_FAIL "true")

#add_test (NAME HL_TOOLS_h52gifpal COMMAND $<TARGET_FILE:h52gif> testfiles/h52giftst.h5 image.gif -i palette)
#set_tests_properties (HL_TOOLS_h52gifpal PROPERTIES WILL_FAIL "true")

add_test (NAME HL_TOOLS_h52gif24bits COMMAND $<TARGET_FILE:h52gif> testfiles/ex_image2.h5 image24.gif -i image24bitpixel)
set_tests_properties (HL_TOOLS_h52gif24bits PROPERTIES WILL_FAIL "true")
