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


##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_H5_TEST resultfile resultcode)
    add_test (
        NAME H5LIBTEST-${resultfile}-clear-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${resultfile}.out
            ${resultfile}.out.err
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5LIBTEST-${resultfile}-clear-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    add_test (NAME H5LIBTEST-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5tools_test_utils> ${ARGN})
    if (NOT "${resultcode}" STREQUAL "0")
      set_tests_properties (H5LIBTEST-${resultfile} PROPERTIES WILL_FAIL "true")
    endif ()
    set_tests_properties (H5LIBTEST-${resultfile} PROPERTIES DEPENDS H5LIBTEST-${resultfile}-clear-objects)
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################
  ADD_H5_TEST (h5tools_utils-default 0)
