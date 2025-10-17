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

include (${HDF_CONFIG_DIR}/HDF5Macros.cmake)

# System-independent path separator
if (WIN32)
  set (CMAKE_SEP "\;")
else ()
  set (CMAKE_SEP ":")
endif ()

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (LIST_HDF5_TEST_FILES
    h5copy_extlinks_src.h5
    h5copy_extlinks_trg.h5
    h5copy_ref.h5
    h5copytst.h5
    tudfilter.h5
     tudfilter2.h5
)

set (LIST_OTHER_TEST_FILES
    h5copy_misc1.out
    tudfilter.h5.txt
    tudfilter.h5_ERR.txt
    h5copy_plugin_fail_ERR.out.h5.txt
    h5copy_plugin_test.out.h5.txt
    h5copy_help1.ddl
    h5copy_help2.ddl
)

file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

# Generate testfiles for VOL connector(s), if any
set (h5copy_vol_files_list "")

foreach (external_vol_tgt ${HDF5_EXTERNAL_VOL_TARGETS})
  HDF5_GET_VOL_TGT_INFO (${external_vol_tgt} ext_vol_dir_name vol_env)

  # Setup testfiles directory
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles" RESULT)
  if (NOT ${RESULT} EQUAL 0)
    message (FATAL_ERROR "Could not create directory ${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles")
  endif()

  add_test(NAME ${external_vol_tgt}-h5copygentest COMMAND $<TARGET_FILE:h5gentest> --h5copy)

  set_tests_properties (${external_vol_tgt}-h5copygentest PROPERTIES
      ENVIRONMENT "${vol_env};${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles"
      FIXTURES_SETUP ${external_vol_tgt}-files
  )

  # These aren't HDF5 files, just copy them to the VOL's subdirectory
  foreach (listothers ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE ("${PROJECT_SOURCE_DIR}/expected/${listothers}"
        "${PROJECT_BINARY_DIR}/${ext_vol_dir_name}/testfiles/${listothers}"
        "h5copy_vol_files"
    )
  endforeach ()
endforeach ()
add_custom_target (h5copy_vol_files ALL COMMENT "Copying files needed by h5copy tests" DEPENDS ${h5copy_vol_files_list})

# Copy pre-existing files for Native tests  
foreach (listfiles ${LIST_HDF5_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${listfiles}" "h5copy_files")
endforeach ()

foreach (listothers ${LIST_OTHER_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5copy/expected/${listothers}" "${PROJECT_BINARY_DIR}/testfiles/${listothers}" "h5copy_files")
endforeach ()
add_custom_target (h5copy_files ALL COMMENT "Copying files needed by h5copy tests" DEPENDS ${h5copy_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

#
# Adds a test for each loaded VOL connector which performs h5copy according to passed parameters
#
# Usage: ADD_H5_TEST(<testname> <required_args> [optional_args] [flags])
#
# REQUIRED POSITIONAL ARGUMENT:
#   testname - name of test to add
#
# REQUIRED KEYWORD ARGUMENTS (must specify value):
#   RESULT_CODE <code>    - expected return code after test execution (0, 1, or 2)
#   INPUT_FILE <file>     - name of input HDF5 file
#   SOURCE_NAME <name>    - source object name/path for h5copy
#   DEST_NAME <name>      - destination object name/path for h5copy
#
# OPTIONAL KEYWORD ARGUMENTS (must specify value):
#   ERROR_CHECK <file>    - if provided, compare error output to this reference file
#   FORCE_FLAG <value>    - value to pass to h5copy's "-f" argument (e.g., "ref", "ext")
#   PREFILL_SRC <name>    - source object for optional prefill test (requires PREFILL_DEST)
#   PREFILL_DEST <name>   - destination object for optional prefill test (requires PREFILL_SRC)
#
# OPTIONAL FLAGS (no value, presence indicates true):
#   SAME_FILE            - use output file as input for main test (for same-file operations)
#   VERBOSE              - add "-v" verbose flag to h5copy command
#
# NOTES:
#   - PREFILL_SRC and PREFILL_DEST must be used together or not at all
#   - RESULT_CODE=2 will skip the diff verification test
#   - Additional arguments can be passed after all keywords (e.g., "-p" flag)
#
macro (ADD_H5_TEST testname)
  cmake_parse_arguments (ARG
      "SAME_FILE;VERBOSE"
      "RESULT_CODE;ERROR_CHECK;INPUT_FILE;FORCE_FLAG;PREFILL_SRC;PREFILL_DEST;SOURCE_NAME;DEST_NAME"
      ""
      ${ARGN}
  )

  # Validate required parameters
  if (NOT DEFINED ARG_RESULT_CODE)
    message (FATAL_ERROR "ADD_H5_TEST: RESULT_CODE is required")
  endif ()
  if (NOT DEFINED ARG_INPUT_FILE)
    message (FATAL_ERROR "ADD_H5_TEST: INPUT_FILE is required")
  endif ()
  if (NOT DEFINED ARG_SOURCE_NAME)
    message (FATAL_ERROR "ADD_H5_TEST: SOURCE_NAME is required")
  endif ()
  if (NOT DEFINED ARG_DEST_NAME)
    message (FATAL_ERROR "ADD_H5_TEST: DEST_NAME is required")
  endif ()

  # Set up force flag
  if (DEFINED ARG_FORCE_FLAG AND NOT "${ARG_FORCE_FLAG}" STREQUAL "")
    set (fparam_flag "-f")
    set (fparam "${ARG_FORCE_FLAG}")
  else ()
    set (fparam_flag "")
    set (fparam "")
  endif ()

  # Set up verbose flag
  if (ARG_VERBOSE)
    set (vparam "-v")
  else ()
    set (vparam "")
  endif ()

  # Determine input file for main test
  if (ARG_SAME_FILE)
    set (main_infile "${testname}.out.h5")
  else ()
    set (main_infile "${ARG_INPUT_FILE}")
  endif ()

  list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_prefix "")
    else ()
      # An external VOL connector
      set (vol_env "")

      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")
      set (vol_workdir "${PROJECT_BINARY_DIR}/${vol}")
      set (vol_fixtures "${vol_prefix}files")
    endif ()

    # Remove any output file left over from previous test run
    add_test (
        NAME ${vol_prefix}H5COPY-${testname}-clear-objects
        COMMAND $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (${vol_prefix}H5COPY-${testname}-clear-objects PROPERTIES
        # h5delete will return an error code if targeted file does not exist - accept any result
        PASS_REGULAR_EXPRESSION "^$|"
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
    )
    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5COPY-${testname}-clear-objects PROPERTIES
          ENVIRONMENT "${vol_env}"
          WORKING_DIRECTORY "${vol_workdir}"
          FIXTURES_REQUIRED "${vol_fixtures}"
      )
    endif ()

    # Optional prefill sub-test, to be done before main test
    if (DEFINED ARG_PREFILL_SRC OR DEFINED ARG_PREFILL_DEST)
      if (NOT DEFINED ARG_PREFILL_SRC OR NOT DEFINED ARG_PREFILL_DEST)
        message (FATAL_ERROR "Prefill test requires both PREFILL_SRC and PREFILL_DEST")
      endif ()
      add_test (
        NAME ${vol_prefix}H5COPY-${testname}-prefill
        COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${ARG_INPUT_FILE} -o ./testfiles/${testname}.out.h5 -v -s ${ARG_PREFILL_SRC} -d ${ARG_PREFILL_DEST}
      )
      set_tests_properties (${vol_prefix}H5COPY-${testname}-prefill PROPERTIES
          DEPENDS ${vol_prefix}H5COPY-${testname}-clear-objects
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY-${testname}-prefill PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()
      if ("${vol_prefix}H5COPY-${testname}-prefill" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5COPY-${testname}-prefill PROPERTIES DISABLED true)
      endif ()

      set (prefill_dep "${vol_prefix}H5COPY-${testname}-prefill")
    else ()
      # No prefill dependency
      set (prefill_dep "")
    endif () # end prefill step

    # No error check
    # Force this behavior if memchecker enabled
    if (NOT DEFINED ARG_ERROR_CHECK OR "${ARG_ERROR_CHECK}" STREQUAL "" OR HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME ${vol_prefix}H5COPY-${testname}
          COMMAND $<TARGET_FILE:h5copy> ${fparam_flag} ${fparam} -i ./testfiles/${main_infile} -o ./testfiles/${testname}.out.h5 ${vparam} -s ${ARG_SOURCE_NAME} -d ${ARG_DEST_NAME} ${ARG_UNPARSED_ARGUMENTS}
      )
    else () # Perform the same test with error checking
      add_test (
        NAME ${vol_prefix}H5COPY-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
            -D "TEST_ARGS=${fparam_flag};${fparam};-i;./testfiles/${main_infile};-o;./testfiles/${testname}.out.h5;${vparam};-s;${ARG_SOURCE_NAME};-d;${ARG_DEST_NAME}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=./testfiles/${testname}.out.out"
            -D "TEST_EXPECT=${ARG_RESULT_CODE}"
            -D "TEST_REFERENCE=./testfiles/${testname}.out"
            -D "TEST_ERRREF=${ARG_ERROR_CHECK}"
            -D "TEST_MASK_STORE=true"
            -D "TEST_GREP_COMPARE=TRUE"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
    endif ()
    set_tests_properties (${vol_prefix}H5COPY-${testname} PROPERTIES DEPENDS
        "${vol_prefix}H5COPY-${testname}-clear-objects;${prefill_dep}"
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
    )
    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5COPY-${testname} PROPERTIES
          ENVIRONMENT "${vol_env}"
          WORKING_DIRECTORY "${vol_workdir}"
          FIXTURES_REQUIRED "${vol_fixtures}"
      )
    endif ()
    if ("${vol_prefix}H5COPY-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (${vol_prefix}H5COPY-${testname} PROPERTIES DISABLED true)
    endif ()

    # Skip diff test if result code is 2
    if (NOT "${ARG_RESULT_CODE}" STREQUAL "2")
      add_test (
          NAME ${vol_prefix}H5COPY-${testname}-DIFF
          COMMAND $<TARGET_FILE:h5diff> -v ./testfiles/${main_infile} ./testfiles/${testname}.out.h5 ${ARG_SOURCE_NAME} ${ARG_DEST_NAME}
      )
      set_tests_properties (${vol_prefix}H5COPY-${testname}-DIFF PROPERTIES
          DEPENDS ${vol_prefix}H5COPY-${testname}
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY-${testname}-DIFF PROPERTIES
            DEPENDS ${vol_prefix}H5COPY-${testname}
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()
      if ("${ARG_RESULT_CODE}" STREQUAL "1")
        set_tests_properties (${vol_prefix}H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("${vol_prefix}H5COPY-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5COPY-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()

    add_test (
        NAME ${vol_prefix}H5COPY-${testname}-clean-objects
        COMMAND $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (${vol_prefix}H5COPY-${testname}-clean-objects PROPERTIES
        # h5delete will return an error code if targeted file does not exist - accept any result
        PASS_REGULAR_EXPRESSION "^$|"
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
    )
    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5COPY-${testname}-clean-objects PROPERTIES
          ENVIRONMENT "${vol_env}"
          WORKING_DIRECTORY "${vol_workdir}"
          FIXTURES_REQUIRED "${vol_fixtures}"
      )
    endif ()
    if (NOT "${ARG_RESULT_CODE}" STREQUAL "2")
      set_tests_properties (${vol_prefix}H5COPY-${testname}-clean-objects PROPERTIES DEPENDS ${vol_prefix}H5COPY-${testname}-DIFF)
    else ()
      set_tests_properties (${vol_prefix}H5COPY-${testname}-clean-objects PROPERTIES DEPENDS ${vol_prefix}H5COPY-${testname})
    endif ()
  endforeach () # per-VOL loop
endmacro ()

macro (ADD_H5_UD_TEST testname resultcode infile sparam srcname dparam dstname cmpfile)
  list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_prefix "")
    else ()
      # An external VOL connector
      set (vol_env "")

      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")
      set (vol_workdir "${PROJECT_BINARY_DIR}/${vol}")
      set (vol_fixtures "${vol_prefix}files")

      # Isolate plugin path string
      string (FIND "${vol_env}" "HDF5_PLUGIN_PATH=" vol_plugin_path_posn)
      if (vol_plugin_path_posn GREATER -1)
        # Grab path string after HDF5_PLUGIN_PATH=
        string (LENGTH "HDF5_PLUGIN_PATH=" path_prefix_len)
        math (EXPR vol_plugin_path_posn "${vol_plugin_path_posn} + ${path_prefix_len}")
        string (SUBSTRING "${vol_env}" ${vol_plugin_path_posn} -1 vol_plugin_path)
      else ()
        set (vol_plugin_path "")
      endif ()
    endif ()

    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME ${vol_prefix}H5COPY_UD-${testname}-clear-objects
          COMMAND $<TARGET_FILE:h5delete> testfiles/${testname}.out.h5
      )
      set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-clear-objects PROPERTIES
          # h5delete will return an error code if targeted file does not exist - accept any result
          PASS_REGULAR_EXPRESSION "^$|"
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-clear-objects PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()

      if ("${resultcode}" STREQUAL "2")
        # force a plugin not found error
        set (ud_search_path ${CMAKE_BINARY_DIR})
      else ()
        # use correct search path
        set (ud_search_path ${CMAKE_BINARY_DIR}/plugins)
      endif ()

      add_test (
        NAME ${vol_prefix}H5COPY_UD-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
            -D "TEST_ARGS:STRING=-v;-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${sparam};${srcname};${dparam};${dstname}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=./testfiles/${infile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=./testfiles/${infile}.txt"
            -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
            -D "TEST_ENV_VALUE=${ud_search_path}${CMAKE_SEP}${vol_plugin_path}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${vol_prefix}H5COPY_UD-${testname} PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD-${testname}-clear-objects
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD-${testname} PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()
      if ("${vol_prefix}H5COPY_UD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5COPY_UD-${testname} PROPERTIES DISABLED true)
      endif ()

      add_test (
          NAME ${vol_prefix}H5COPY_UD-${testname}-DIFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;./testfiles/${cmpfile};./testfiles/${testname}.out.h5;${srcname};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.h5.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out.h5.txt"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins${CMAKE_SEP}${vol_plugin_path}"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-DIFF PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD-${testname}
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )

      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-DIFF PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()
      if ("${vol_prefix}H5COPY_UD-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()

      add_test (
          NAME ${vol_prefix}H5COPY_UD-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}.out.h5
      )
      set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-clean-objects PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD-${testname}-DIFF
      )
      set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-clean-objects PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD-${testname}-DIFF
          # h5delete will return an error code if targeted file does not exist - accept any result
          PASS_REGULAR_EXPRESSION "^$|"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD-${testname}-clean-objects PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()

    endif ()
  endforeach () # per-VOL loop
endmacro ()

macro (ADD_H5_UD_ERR_TEST testname resultcode infile sparam srcname dparam dstname cmpfile)
  list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_prefix "")
    else ()
      # An external VOL connector
      set (vol_env "")

      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")
      set (vol_workdir "${PROJECT_BINARY_DIR}/${vol}")
      set (vol_fixtures "${vol_prefix}files")

      # Isolate plugin path string
      string (FIND "${vol_env}" "HDF5_PLUGIN_PATH=" vol_plugin_path_posn)
      if (vol_plugin_path_posn GREATER -1)
        # Grab path string after HDF5_PLUGIN_PATH=
        string (LENGTH "HDF5_PLUGIN_PATH=" path_prefix_len)
        math (EXPR vol_plugin_path_posn "${vol_plugin_path_posn} + ${path_prefix_len}")
        string (SUBSTRING "${vol_env}" ${vol_plugin_path_posn} -1 vol_plugin_path)
      else ()
        set (vol_plugin_path "")
      endif ()
    endif ()

    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME ${vol_prefix}H5COPY_UD_ERR-${testname}-clear-objects
          COMMAND $<TARGET_FILE:h5delete> testfiles/${testname}_ERR.out.h5
      )
      set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-clear-objects PROPERTIES
          # h5delete will return an error code if targeted file does not exist - accept any result
          PASS_REGULAR_EXPRESSION "^$|"
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-clear-objects PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()

      if ("${resultcode}" STREQUAL "2")
        # force a plugin not found error
        set (ud_search_path ${CMAKE_BINARY_DIR})
        set (expected_result "0")
      else ()
        # use correct search path
        set (ud_search_path ${CMAKE_BINARY_DIR}/plugins)
        set (expected_result "${resultcode}")
      endif ()

      add_test (
        NAME ${vol_prefix}H5COPY_UD_ERR-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
            -D "TEST_ARGS:STRING=-v;--enable-error-stack;-i;./testfiles/${infile};-o;./testfiles/${testname}_ERR.out.h5;${sparam};${srcname};${dparam};${dstname}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=./testfiles/${infile}_ERR.out"
            -D "TEST_EXPECT=${expected_result}"
            -D "TEST_REFERENCE=./testfiles/${infile}_ERR.txt"
            -D "TEST_MASK_ERROR=true"
            -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
            -D "TEST_ENV_VALUE=${ud_search_path}${CMAKE_SEP}${vol_plugin_path}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname} PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD_ERR-${testname}-clear-objects
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname} PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()
      if ("${vol_prefix}H5COPY_UD_ERR-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname} PROPERTIES DISABLED true)
      endif ()

      add_test (
          NAME ${vol_prefix}H5COPY_UD_ERR-${testname}-DIFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;./testfiles/${cmpfile};./testfiles/${testname}_ERR.out.h5;${srcname};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}_ERR.out.h5.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=./testfiles/${testname}_ERR.out.h5.txt"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins${CMAKE_SEP}${vol_plugin_path}"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-DIFF PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD_ERR-${testname}
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-DIFF PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()
      if ("${vol_prefix}H5COPY_UD_ERR-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()

      add_test (
          NAME ${vol_prefix}H5COPY_UD_ERR-${testname}-clean-objects
          COMMAND $<TARGET_FILE:h5delete> testfiles/${testname}_ERR.out.h5
      )
      set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-clean-objects PROPERTIES
          # h5delete will return an error code if targeted file does not exist - accept any result
          PASS_REGULAR_EXPRESSION "^$|"
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-clean-objects PROPERTIES
          DEPENDS ${vol_prefix}H5COPY_UD_ERR-${testname}-DIFF
      )
      if (NOT "${vol}" STREQUAL "native")
        set_tests_properties (${vol_prefix}H5COPY_UD_ERR-${testname}-clean-objects PROPERTIES
            ENVIRONMENT "${vol_env}"
            WORKING_DIRECTORY "${vol_workdir}"
            FIXTURES_REQUIRED "${vol_fixtures}"
        )
      endif ()

    endif ()
  endforeach () # per-VOL loop
endmacro ()

macro (ADD_SIMPLE_TEST resultfile resultcode)
  list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_prefix "")
    else ()
      # An external VOL connector
      set (vol_env "")

      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")
      set (vol_workdir "${PROJECT_BINARY_DIR}/${vol}")
      set (vol_fixtures "${vol_prefix}files")
    endif ()

    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME ${vol_prefix}H5COPY-${resultfile} COMMAND $<TARGET_FILE:h5copy> ${ARGN})
      if (${resultcode})
        set_tests_properties (${vol_prefix}H5COPY-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME ${vol_prefix}H5COPY-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${resultfile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (${vol_prefix}H5COPY-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
    )
    if (NOT "${vol}" STREQUAL "native")
      set_tests_properties (${vol_prefix}H5COPY-${resultfile} PROPERTIES
          ENVIRONMENT "${vol_env}"
          WORKING_DIRECTORY "${vol_workdir}"
          FIXTURES_REQUIRED "${vol_fixtures}"
      )
    endif ()
    if ("${vol_prefix}H5COPY-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (${vol_prefix}H5COPY-${resultfile} PROPERTIES DISABLED true)
    endif ()

  endforeach () # per-VOL loop
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# test file names
# --------------------------------------------------------------------
set (HDF_FILE1 h5copytst)
set (HDF_FILE2 h5copy_ref)
set (HDF_EXT_SRC_FILE h5copy_extlinks_src)
set (HDF_EXT_TRG_FILE h5copy_extlinks_trg)

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
if (H5_HAVE_FILTER_DEFLATE)
  set (USE_FILTER_DEFLATE "true")
endif ()

if (H5_HAVE_FILTER_SZIP)
  set (USE_FILTER_SZIP "true")
endif ()

# Test for help flag
ADD_SIMPLE_TEST (h5copy_help1 0 -h)
ADD_SIMPLE_TEST (h5copy_help2 0 --help)

# "Test copying various forms of datasets"
ADD_H5_TEST (simple RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME simple DEST_NAME simple)
ADD_H5_TEST (chunk RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME chunk DEST_NAME chunk)
ADD_H5_TEST (compact RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME compact DEST_NAME compact)
ADD_H5_TEST (compound RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME compound DEST_NAME compound)

if (USE_FILTER_DEFLATE)
  ADD_H5_TEST (compressed RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME compressed DEST_NAME compressed)
else ()
  ADD_H5_TEST (compressed RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME compressed DEST_NAME compressed)
endif ()

ADD_H5_TEST (named_vl RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME named_vl DEST_NAME named_vl)
ADD_H5_TEST (nested_vl RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME nested_vl DEST_NAME nested_vl)
ADD_H5_TEST (dset_attr RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME dset_attr DEST_NAME dset_attr)

# "Test copying dataset within group in source file to root of destination"
ADD_H5_TEST (simple_top RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_dsets/simple DEST_NAME simple_top)

# "Test copying & renaming dataset"
ADD_H5_TEST (dsrename RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME compound DEST_NAME rename)

# "Test copying empty, 'full' & 'nested' groups"
ADD_H5_TEST (grp_empty RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_empty DEST_NAME grp_empty)
if (USE_FILTER_DEFLATE)
  ADD_H5_TEST (grp_dsets RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_dsets DEST_NAME grp_dsets)
  ADD_H5_TEST (grp_nested RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_nested DEST_NAME grp_nested)
else ()
  ADD_H5_TEST (grp_dsets RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_dsets DEST_NAME grp_dsets)
  ADD_H5_TEST (grp_nested RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_nested DEST_NAME grp_nested)
endif ()
ADD_H5_TEST (grp_attr RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_attr DEST_NAME grp_attr)

# "Test copying dataset within group in source file to group in destination"
ADD_H5_TEST (simple_group RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 PREFILL_SRC grp_dsets PREFILL_DEST grp_dsets VERBOSE SOURCE_NAME /grp_dsets/simple DEST_NAME /grp_dsets/simple_group)

if (USE_FILTER_DEFLATE)
  # "Test copying & renaming group"
  ADD_H5_TEST (grp_rename RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_dsets DEST_NAME grp_rename)
  # "Test copying 'full' group hierarchy into group in destination file"
  ADD_H5_TEST (grp_dsets_rename RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 PREFILL_SRC grp_dsets PREFILL_DEST grp_rename VERBOSE SOURCE_NAME grp_dsets DEST_NAME /grp_rename/grp_dsets)
else ()
  # "Test copying & renaming group"
  ADD_H5_TEST (grp_rename RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME grp_dsets DEST_NAME grp_rename)
  # "Test copying 'full' group hierarchy into group in destination file"
  ADD_H5_TEST (grp_dsets_rename RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 PREFILL_SRC grp_dsets PREFILL_DEST grp_rename VERBOSE SOURCE_NAME grp_dsets DEST_NAME /grp_rename/grp_dsets)
endif ()

# "Test copying objects into group that doesn't exist yet in destination file"
ADD_H5_TEST (A_B1_simple RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME simple DEST_NAME /A/B1/simple -p)
ADD_H5_TEST (A_B2_simple2 RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME simple DEST_NAME /A/B2/simple2 -p)
ADD_H5_TEST (C_D_simple RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME /grp_dsets/simple DEST_NAME /C/D/simple -p)
if (USE_FILTER_DEFLATE)
  ADD_H5_TEST (E_F_grp_dsets RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME /grp_dsets DEST_NAME /E/F/grp_dsets -p)
  ADD_H5_TEST (G_H_grp_nested RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME /grp_nested DEST_NAME /G/H/grp_nested -p)
else ()
  ADD_H5_TEST (E_F_grp_dsets RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME /grp_dsets DEST_NAME /E/F/grp_dsets -p)
  ADD_H5_TEST (G_H_grp_nested RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME /grp_nested DEST_NAME /G/H/grp_nested -p)
endif ()

############# COPY REFERENCES ##############

# "Test copying object and region references"
ADD_H5_TEST (region_ref RESULT_CODE 2 INPUT_FILE ${HDF_FILE2}.h5 FORCE_FLAG ref VERBOSE SOURCE_NAME / DEST_NAME /COPY)

############# COPY EXT LINKS ##############

# "Test copying external link directly without -f ext"
ADD_H5_TEST (ext_link RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 VERBOSE SOURCE_NAME /group_ext/extlink_dset DEST_NAME /copy1_dset)

# "Test copying external link directly with -f ext"
ADD_H5_TEST (ext_link_f RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 FORCE_FLAG ext VERBOSE SOURCE_NAME /group_ext/extlink_dset DEST_NAME /copy2_dset)

# "Test copying dangling external link (no obj) directly without -f ext"
ADD_H5_TEST (ext_dangle_noobj RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 VERBOSE SOURCE_NAME /group_ext/extlink_notyet1 DEST_NAME /copy_dangle1_1)

# "Test copying dangling external link (no obj) directly with -f ext"
ADD_H5_TEST (ext_dangle_noobj_f RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 FORCE_FLAG ext VERBOSE SOURCE_NAME /group_ext/extlink_notyet1 DEST_NAME /copy_dangle1_2)

# "Test copying dangling external link (no file) directly without -f ext"
ADD_H5_TEST (ext_dangle_nofile RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 VERBOSE SOURCE_NAME /group_ext/extlink_notyet2 DEST_NAME /copy_dangle2_1)

# "Test copying dangling external link (no file) directly with -f ext"
ADD_H5_TEST (ext_dangle_nofile_f RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 FORCE_FLAG ext VERBOSE SOURCE_NAME /group_ext/extlink_notyet2 DEST_NAME /copy_dangle2_2)

# "Test copying a group contains external links without -f ext"
ADD_H5_TEST (ext_link_group RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 VERBOSE SOURCE_NAME /group_ext DEST_NAME /copy1_group)

# "Test copying a group contains external links with -f ext"
ADD_H5_TEST (ext_link_group_f RESULT_CODE 2 INPUT_FILE ${HDF_EXT_SRC_FILE}.h5 FORCE_FLAG ext VERBOSE SOURCE_NAME /group_ext DEST_NAME /copy2_group)

############# Test misc. ##############

#-----------------------------------------------------------------
# "Test copying object into group which doesn't exist, without -p"
#
ADD_H5_TEST (h5copy_misc1 RESULT_CODE 1 ERROR_CHECK "h5copy error" INPUT_FILE ${HDF_FILE1}.h5 VERBOSE SOURCE_NAME /simple DEST_NAME /g1/g2/simple)

#-------------------------------------------
# "Test copying objects to the same file "
#
# - dataset
ADD_H5_TEST (samefile1 RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 SAME_FILE PREFILL_SRC /simple PREFILL_DEST /simple VERBOSE SOURCE_NAME /simple DEST_NAME /simple_cp)
# - group with some datasets
if (USE_FILTER_DEFLATE)
  ADD_H5_TEST (samefile2 RESULT_CODE 0 INPUT_FILE ${HDF_FILE1}.h5 SAME_FILE PREFILL_SRC /grp_dsets PREFILL_DEST /grp_dsets VERBOSE SOURCE_NAME /grp_dsets DEST_NAME /grp_dsets_cp)
else ()
  ADD_H5_TEST (samefile2 RESULT_CODE 2 INPUT_FILE ${HDF_FILE1}.h5 SAME_FILE PREFILL_SRC /grp_dsets PREFILL_DEST /grp_dsets VERBOSE SOURCE_NAME /grp_dsets DEST_NAME /grp_dsets_cp)
endif ()

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (h5copy_plugin_test 0 tudfilter.h5 -s /dynlibud -d /dynlibud tudfilter2.h5 )
  ADD_H5_UD_ERR_TEST (h5copy_plugin_fail 2 tudfilter.h5 -s /dynlibud -d /dynlibud tudfilter2.h5)
endif ()
