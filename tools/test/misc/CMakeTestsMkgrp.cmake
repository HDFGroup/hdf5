#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (HDF5_MKGRP_TEST_FILES
    #h5mkgrp_help.txt
    #h5mkgrp_version
    h5mkgrp_single.ls
    h5mkgrp_single_v.ls
    h5mkgrp_single_p.ls
    h5mkgrp_single_l.ls
    h5mkgrp_several.ls
    h5mkgrp_several_v.ls
    h5mkgrp_several_p.ls
    h5mkgrp_several_l.ls
    h5mkgrp_nested_p.ls
    h5mkgrp_nested_lp.ls
    h5mkgrp_nested_mult_p.ls
    h5mkgrp_nested_mult_lp.ls
)

# make test dir
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

foreach (h5_mkgrp_file ${HDF5_MKGRP_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/misc/expected/${h5_mkgrp_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_mkgrp_file}" "h5mkgrp_files")
endforeach ()

HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/misc/expected/h5mkgrp_help.txt" "${PROJECT_BINARY_DIR}/testfiles/h5mkgrp_help.txt" "h5mkgrp_files")
add_custom_target (h5mkgrp_files ALL COMMENT "Copying files needed by h5mkgrp tests" DEPENDS ${h5mkgrp_files_list})

configure_file (${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/testfiles/h5mkgrp_version.txt.in ${PROJECT_BINARY_DIR}/testfiles/h5mkgrp_version.txt @ONLY)

#  Generate testfiles for VOL connector(s), if any
set (h5mkgrp_vol_files_list "")

foreach (external_vol_tgt ${HDF5_EXTERNAL_VOL_TARGETS})
  HDF5_GET_VOL_TGT_INFO (${external_vol_tgt} vol vol_env)

  # Setup testfiles directory
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vol}/testfiles" RESULT)
  if (NOT ${RESULT} EQUAL 0)
    message (FATAL_ERROR "Failed to create directory: ${PROJECT_BINARY_DIR}/${vol}/testfiles")
  endif ()

  # h5mkgrp depends on no pre-existing HDF5 files; no need to use h5gentest here

  # Copy expected output files
  foreach (h5_mkgrp_file ${HDF5_MKGRP_TEST_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/misc/expected/${h5_mkgrp_file}"
        "${PROJECT_BINARY_DIR}/${vol}/testfiles/${h5_mkgrp_file}"
        "h5mkgrp_vol_files"
    )
  endforeach ()
endforeach ()

add_custom_target (h5mkgrp_vol_files ALL COMMENT "Copying files needed by h5mkgrp VOL tests" DEPENDS ${h5mkgrp_vol_files_list})
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

#
# Adds a test that performs h5mkgrp according to given parameters
#
# REQUIRED POSITIONAL ARGUMENTS:
#  testname  - name of the test (used to name the test and output files)
#
# REQUIRED KEYWORD ARGUMENTS:
#  RESULT_CODE <resultcode> - expected return code from h5mkgrp
#
# OPTIONAL KEYWORD ARGUMENTS:
#  RESULT_OPTION <flag> - a flag to pass to h5mkgrp
#
macro (ADD_H5_TEST testname)
  # === Argument processing  ===
  cmake_parse_arguments (
      ARG
      "" # flags
      "RESULT_CODE;RESULT_OPTION" # one value args
      "" # multi value args
      ${ARGN}
  )

  if (NOT DEFINED ARG_RESULT_CODE)
    message (FATAL_ERROR "ADD_H5_TEST: RESULT_CODE must be defined")
  endif ()

  if (NOT DEFINED ARG_RESULT_OPTION)
    set (ARG_RESULT_OPTION "")
  endif ()

  # === Adding the Test ===
  list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_prefix "")
      set (vol_workdir "${PROJECT_BINARY_DIR}/testfiles")
    else ()
      # An external VOL connector
      set (vol_env "")

      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")
      set (vol_workdir "${PROJECT_BINARY_DIR}/${vol}/testfiles")
      set (vol_fixtures "${vol_prefix}files")
    endif ()

    # == Clean up ==
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME ${vol_prefix}H5MKGRP-${testname}-clear-objects
          COMMAND $<TARGET_FILE:h5delete> ${testname}.h5
      )

      set_tests_properties (${vol_prefix}H5MKGRP-${testname}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${vol_workdir}"
          # h5delete will return an error code if targeted file does not exist - accept any result
          PASS_REGULAR_EXPRESSION "^$|"
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )

      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5MKGRP-${testname}-clear-objects PROPERTIES
            DEPENDS h5mkgrp_vol_files
            ENVIRONMENT "${vol_env}"
        )
      endif ()
    endif ()

    # == Main test ==
    add_test (
        NAME ${vol_prefix}H5MKGRP-${testname}
        COMMAND $<TARGET_FILE:h5mkgrp> ${ARG_RESULT_OPTION} ${testname}.h5 ${ARG_UNPARSED_ARGUMENTS}
    )

    if ("${vol_prefix}H5MKGRP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (${vol_prefix}H5MKGRP-${testname} PROPERTIES DISABLED true)
    endif ()

    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      set_tests_properties (${vol_prefix}H5MKGRP-${testname} PROPERTIES
          DEPENDS ${vol_prefix}H5MKGRP-${testname}-clear-objects
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
    endif()

    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5MKGRP-${testname} PROPERTIES
          DEPENDS h5mkgrp_vol_files
          ENVIRONMENT "${vol_env}"
      )
    endif ()

    # == Verify with h5ls ==
    add_test (
      NAME ${vol_prefix}H5MKGRP-${testname}-h5ls
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
          -D "TEST_ARGS:STRING=-v;-r;${testname}.h5"
          -D "TEST_FOLDER=${vol_workdir}"
          -D "TEST_OUTPUT=${testname}.out"
          -D "TEST_EXPECT=${ARG_RESULT_CODE}"
          -D "TEST_REFERENCE=${testname}.ls"
          -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )

    set_tests_properties (${vol_prefix}H5MKGRP-${testname}-h5ls PROPERTIES
        DEPENDS ${vol_prefix}H5MKGRP-${testname}
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
    )

    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5MKGRP-${testname}-h5ls PROPERTIES
          DEPENDS h5mkgrp_vol_files
          ENVIRONMENT "${vol_env}"
      )
    endif ()

    if ("${vol_prefix}H5MKGRP-${testname}-h5ls" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (${vol_prefix}H5MKGRP-${testname}-h5ls PROPERTIES DISABLED true)
    endif ()

    set_tests_properties ("${vol_prefix}H5MKGRP-${testname}" PROPERTIES
        WORKING_DIRECTORY "${vol_workdir}"
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
    )
  endforeach () # per-VOL loop
endmacro ()

macro (ADD_H5_CMP resultfile resultcode)
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5MKGRP_CMP-${resultfile} COMMAND $<TARGET_FILE:h5mkgrp> ${ARGN})
    set_tests_properties (H5MKGRP_CMP-${resultfile} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
  else ()
    add_test (
        NAME H5MKGRP_CMP-${resultfile}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}.h5
    )
    set_tests_properties (H5MKGRP_CMP-${resultfile}-clear-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    add_test (
        NAME H5MKGRP_CMP-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5mkgrp>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.txt"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5MKGRP_CMP-${resultfile} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5MKGRP_CMP-${resultfile}-clear-objects
    )
    if ("H5MKGRP_CMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5MKGRP_CMP-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (
      NAME H5MKGRP-clearall-objects
      COMMAND ${CMAKE_COMMAND} -E remove
          h5mkgrp_single.h5
          h5mkgrp_single_v.h5
          h5mkgrp_single_p.h5
          h5mkgrp_single_l.h5
          h5mkgrp_several.h5
          h5mkgrp_several_v.h5
          h5mkgrp_several_p.h5
          h5mkgrp_several_l.h5
          h5mkgrp_nested_p.h5
          h5mkgrp_nested_lp.h5
          h5mkgrp_nested_mult_p.h5
          h5mkgrp_nested_mult_lp.h5
  )
  set_tests_properties (H5MKGRP-clearall-objects PROPERTIES
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
endif ()

# Check that help & version is displayed properly
ADD_H5_CMP (h5mkgrp_help 0 "-h")
ADD_H5_CMP (h5mkgrp_version 0 "-V")

# Create single group at root level
ADD_H5_TEST (h5mkgrp_single RESULT_CODE 0 single)
ADD_H5_TEST (h5mkgrp_single_v RESULT_CODE 0 RESULT_OPTION "-v" single)
ADD_H5_TEST (h5mkgrp_single_p RESULT_CODE 0 RESULT_OPTION "-p" single)
ADD_H5_TEST (h5mkgrp_single_l RESULT_CODE 0 RESULT_OPTION "-l" latest)

# Create several groups at root level
ADD_H5_TEST (h5mkgrp_several RESULT_CODE 0 one two)
ADD_H5_TEST (h5mkgrp_several_v RESULT_CODE 0 RESULT_OPTION "-v" one two)
ADD_H5_TEST (h5mkgrp_several_p RESULT_CODE 0 RESULT_OPTION "-p" one two)
ADD_H5_TEST (h5mkgrp_several_l RESULT_CODE 0 RESULT_OPTION "-l" one two)

# Create various nested groups
ADD_H5_TEST (h5mkgrp_nested_p RESULT_CODE 0 RESULT_OPTION "-p" /one/two)
ADD_H5_TEST (h5mkgrp_nested_lp RESULT_CODE 0 RESULT_OPTION "-lp" /one/two)
ADD_H5_TEST (h5mkgrp_nested_mult_p RESULT_CODE 0 RESULT_OPTION "-p" /one/two /three/four)
ADD_H5_TEST (h5mkgrp_nested_mult_lp RESULT_CODE 0 RESULT_OPTION "-lp" /one/two /three/four)
