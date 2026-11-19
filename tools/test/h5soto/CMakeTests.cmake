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

file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

set (H5SOTO_REFERENCE_FILES
    count.txt
    verbose.txt
    materialize.txt
    materialized-rev3.ddl
)

set (H5SOTO_INPUT_FILES
    tst_onion_objs.h5
    tst_onion_objs.h5.onion
)

foreach (ref_file ${H5SOTO_REFERENCE_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TEST_H5SOTO_SOURCE_DIR}/expected/${ref_file}" "${PROJECT_BINARY_DIR}/testfiles/${ref_file}" "h5soto_test_files")
endforeach ()

foreach (input_file ${H5SOTO_INPUT_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${input_file}" "${PROJECT_BINARY_DIR}/testfiles/${input_file}" "h5soto_test_files")
endforeach ()

add_custom_target (h5soto_test_files ALL COMMENT "Copying files needed by h5soto tests" DEPENDS ${h5soto_test_files_list})

macro (ADD_H5SOTO_TEST testname resultcode reference)
  add_test (
      NAME H5SOTO-${testname}
      COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:h5soto>"
          -D "TEST_ARGS:STRING=${ARGN}"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
          -D "TEST_OUTPUT=h5soto-${testname}.out"
          -D "TEST_EXPECT=${resultcode}"
          -D "TEST_REFERENCE=${reference}"
          -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
  set_tests_properties (H5SOTO-${testname} PROPERTIES
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
endmacro ()

add_h5soto_test (count 0 count.txt tst_onion_objs.h5)
add_h5soto_test (verbose 0 verbose.txt --verbose tst_onion_objs.h5)
add_h5soto_test (materialize_generate 0 materialize.txt --materialize=3 --output=materialized-rev3.h5 tst_onion_objs.h5)

# -------------------------------------------------------------------------
# Error-path tests
# -------------------------------------------------------------------------

set (H5SOTO_REFERENCE_ERR_FILES
    err_no_onion.txt
    err_revision_range.txt
    err_list_range.txt
    err_invalid_rev.txt
    err_output_exists.txt
    err_mutual_excl.txt
)

foreach (ref_file ${H5SOTO_REFERENCE_ERR_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TEST_H5SOTO_SOURCE_DIR}/expected/${ref_file}" "${PROJECT_BINARY_DIR}/testfiles/${ref_file}" "h5soto_test_files")
endforeach ()

add_h5soto_test (err_no_onion 1 err_no_onion.txt tst_onion_objs.h5.onion)
add_h5soto_test (err_revision_range 1 err_revision_range.txt --materialize=999 tst_onion_objs.h5)
add_h5soto_test (err_list_range 1 err_list_range.txt --list=999 tst_onion_objs.h5)
add_h5soto_test (err_invalid_rev 1 err_invalid_rev.txt --materialize=abc tst_onion_objs.h5)
add_h5soto_test (err_mutual_excl 1 err_mutual_excl.txt --verbose --materialize=0 tst_onion_objs.h5)

# Verify that --force allows overwriting an existing output file.
# The fixture creates the target file first, then checks --force succeeds.
add_h5soto_test (err_output_exists 1 err_output_exists.txt --materialize=0 --output=exists-output.h5 tst_onion_objs.h5)
set_tests_properties (H5SOTO-err_output_exists PROPERTIES
    FIXTURES_REQUIRED "h5soto_exists_output"
)

add_test (
    NAME H5SOTO-force_overwrite
    COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:h5soto>"
        -D "TEST_ARGS:STRING=--materialize=0;--force;--output=exists-output.h5;tst_onion_objs.h5"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
        -D "TEST_OUTPUT=h5soto-force_overwrite.out"
        -D "TEST_EXPECT=0"
        -D "TEST_REFERENCE=force_overwrite.txt"
        -D "TEST_DELETE_LIST=${PROJECT_BINARY_DIR}/testfiles/exists-output.h5"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
)
set_tests_properties (H5SOTO-force_overwrite PROPERTIES
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    FIXTURES_REQUIRED "h5soto_exists_output"
    DEPENDS H5SOTO-err_output_exists
)

add_test (
    NAME H5SOTO-err_output_exists_fixture_setup
    COMMAND "${CMAKE_COMMAND}" -E copy
        "${PROJECT_BINARY_DIR}/testfiles/tst_onion_objs.h5"
        "${PROJECT_BINARY_DIR}/testfiles/exists-output.h5"
)
set_tests_properties (H5SOTO-err_output_exists_fixture_setup PROPERTIES
    FIXTURES_SETUP "h5soto_exists_output"
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
)

# --list mode tests
add_h5soto_test (list_rev3 0 list_rev3.txt --list=3 tst_onion_objs.h5)
add_h5soto_test (list_latest 0 list_latest.txt --list=latest tst_onion_objs.h5)

# verbose with --from/--to range
add_h5soto_test (verbose_range 0 verbose_range.txt --verbose --from=3 --to=5 tst_onion_objs.h5)

foreach (ref_file force_overwrite.txt list_rev3.txt list_latest.txt verbose_range.txt)
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TEST_H5SOTO_SOURCE_DIR}/expected/${ref_file}" "${PROJECT_BINARY_DIR}/testfiles/${ref_file}" "h5soto_test_files")
endforeach ()

set_tests_properties (H5SOTO-materialize_generate PROPERTIES
    FIXTURES_SETUP "h5soto_materialized"
)

add_test (
    NAME H5SOTO-materialize_verify
    COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
        -D "TEST_ARGS:STRING=materialized-rev3.h5"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
        -D "TEST_OUTPUT=h5dump-materialized-rev3.out"
        -D "TEST_EXPECT=0"
        -D "TEST_REFERENCE=materialized-rev3.ddl"
        -D "TEST_DELETE_LIST=${PROJECT_BINARY_DIR}/testfiles/materialized-rev3.h5"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
)
set_tests_properties (H5SOTO-materialize_verify PROPERTIES
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    FIXTURES_REQUIRED "h5soto_materialized"
)
