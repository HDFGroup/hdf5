#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

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
HDFTEST_COPY_FILE("${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/testfiles/image1.gif" "${PROJECT_BINARY_DIR}/testfiles/image1.gif" "gif2h5_files")
HDFTEST_COPY_FILE("${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/testfiles/h52giftst.h5" "${PROJECT_BINARY_DIR}/testfiles/h52giftst.h5" "gif2h5_files")
HDFTEST_COPY_FILE("${HDF5_HL_TOOLS_GIF2H5_SOURCE_DIR}/testfiles/ex_image2.h5" "${PROJECT_BINARY_DIR}/testfiles/ex_image2.h5" "gif2h5_files")
add_custom_target(gif2h5_files ALL COMMENT "Copying files needed by gif2h5 tests" DEPENDS ${gif2h5_files_list})

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
set_tests_properties (HL_TOOLS_gif2h5 PROPERTIES DEPENDS HL_TOOLS-clear-objects)

add_test (NAME HL_TOOLS_h52gif COMMAND $<TARGET_FILE:h52gif> testfiles/h52giftst.h5 image1.gif -i image)
set_tests_properties (HL_TOOLS_h52gif PROPERTIES DEPENDS HL_TOOLS-clear-objects)

add_test (NAME HL_TOOLS_h52gif_none COMMAND $<TARGET_FILE:h52gif> testfiles/h52giftst.h5 image.gif -i nosuch_image)
set_tests_properties (HL_TOOLS_h52gif_none PROPERTIES WILL_FAIL "true")
set_tests_properties (HL_TOOLS_h52gif_none PROPERTIES DEPENDS HL_TOOLS-clear-objects)

#add_test (NAME HL_TOOLS_h52gifpal COMMAND $<TARGET_FILE:h52gif> testfiles/h52giftst.h5 image.gif -i palette)
#set_tests_properties (HL_TOOLS_h52gifpal PROPERTIES WILL_FAIL "true")

add_test (NAME HL_TOOLS_h52gif24bits COMMAND $<TARGET_FILE:h52gif> testfiles/ex_image2.h5 image24.gif -i image24bitpixel)
set_tests_properties (HL_TOOLS_h52gif24bits PROPERTIES WILL_FAIL "true")
set_tests_properties (HL_TOOLS_h52gif24bits PROPERTIES DEPENDS HL_TOOLS-clear-objects)
