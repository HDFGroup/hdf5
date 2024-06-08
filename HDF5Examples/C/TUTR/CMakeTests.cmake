#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
file (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)

set (test_ex_CLEANFILES
    Attributes.h5
    btrees_file.h5
    cmprss.h5
    default_file.h5
    dset.h5
    extend.h5
    extlink_prefix_source.h5
    extlink_source.h5
    extlink_target.h5
    group.h5
    groups.h5
    hard_link.h5
    mount1.h5
    mount2.h5
    one_index_file.h5
    only_dspaces_and_attrs_file.h5
    only_huge_mesgs_file.h5
    REF_REG.h5
    refere.h5
    refer_deprec.h5
    refer_extern1.h5
    refer_extern2.h5
    SDS.h5
    SDScompound.h5
    SDSextendible.h5
    Select.h5
    separate_indexes_file.h5
    small_lists_file.h5
    soft_link.h5
    subset.h5
    unix2win.h5
    blue/prefix_target.h5
    red/prefix_target.h5
    u2w/u2w_target.h5
)

if (HDF5_TEST_SERIAL)
  # Remove any output file left over from previous test run
  add_test (
      NAME EXAMPLES-clear-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${test_ex_CLEANFILES}
  )
  set_tests_properties (EXAMPLES-clear-objects PROPERTIES
      FIXTURES_SETUP clear_EXAMPLES
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
  add_test (
      NAME EXAMPLES-clean-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${test_ex_CLEANFILES}
  )
  set_tests_properties (EXAMPLES-clean-objects PROPERTIES
      FIXTURES_CLEANUP clear_EXAMPLES
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  foreach (example ${examples})
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME EXAMPLES-${example} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${example}>)
    else ()
      add_test (NAME EXAMPLES-${example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${example}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${example}.txt"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (EXAMPLES-${example} PROPERTIES FIXTURES_REQUIRED clear_EXAMPLES)
    if (last_test)
      set_tests_properties (EXAMPLES-${example} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "EXAMPLES-${example}")
  endforeach ()
endif ()
