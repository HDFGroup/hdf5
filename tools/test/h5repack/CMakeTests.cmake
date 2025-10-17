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
set (LIST_REPACK_TEST_FILES
    h5repack_f32le_ex-0.dat
    h5repack_int32le_1d_ex-0.dat
    h5repack_int32le_1d_ex-1.dat
    h5repack_int32le_2d_ex-0.dat
    h5repack_int32le_3d_ex-0.dat
    h5repack_uint8be_ex-0.dat
    h5repack_uint8be_ex-1.dat
    h5repack_uint8be_ex-2.dat
    h5repack_uint8be_ex-3.dat
)

set (LIST_REPACK_HDF5_TEST_FILES
    bounds_latest_latest.h5
    h5repack_attr.h5
    h5repack_attr_refs.h5
    h5repack_deflate.h5
    h5repack_early.h5
    h5repack_ext.h5
    h5repack_f32le.h5
    h5repack_f32le_ex.h5
    h5repack_fill.h5
    h5repack_filters.h5
    h5repack_fletcher.h5
    h5repack_hlink.h5
    h5repack_int32le_1d.h5
    h5repack_int32le_1d_ex.h5
    h5repack_int32le_2d.h5
    h5repack_int32le_2d_ex.h5
    h5repack_int32le_3d.h5
    h5repack_int32le_3d_ex.h5
    h5repack_layout.h5
    h5repack_layouto.h5
    h5repack_layout2.h5
    h5repack_layout3.h5
    h5repack_layout.UD.h5
    h5repack_named_dtypes.h5
    h5repack_nested_8bit_enum.h5
    h5repack_nested_8bit_enum_deflated.h5
    h5repack_CVE-2018-17432.h5
    h5repack_CVE-2018-14460.h5
    h5repack_nbit.h5
    h5repack_objs.h5
    h5repack_refs.h5
    h5repack_shuffle.h5
    h5repack_soffset.h5
    h5repack_szip.h5
    h5repack_uint8be.h5
    h5repack_uint8be_ex.h5
    # fsm
    h5repack_aggr.h5
    h5repack_fsm_aggr_nopersist.h5
    h5repack_fsm_aggr_persist.h5
    h5repack_none.h5
    h5repack_paged_nopersist.h5
    h5repack_paged_persist.h5
)
# h5diff/testfile
set (LIST_DIFF_TEST_FILES
    h5diff_attr1.h5
)
# tools/testfiles/vds
set (LIST_VDS_TEST_FILES
    1_a.h5
    1_b.h5
    1_c.h5
    1_d.h5
    1_e.h5
    1_f.h5
    1_vds.h5
    2_a.h5
    2_b.h5
    2_c.h5
    2_d.h5
    2_e.h5
    2_vds.h5
    3_1_vds.h5
    3_2_vds.h5
    4_0.h5
    4_1.h5
    4_2.h5
    4_vds.h5
    5_a.h5
    5_b.h5
    5_c.h5
    5_vds.h5
)
set (LIST_COPY_TEST_FILES
    h5copy_extlinks_src.h5
    h5copy_extlinks_trg.h5
)
set (LIST_HDF5_TEST_FILES
    # tools/testfiles for external links
    tsoftlinks.h5
    textlinkfar.h5
    textlinksrc.h5
    textlinktar.h5
    textlink.h5
    # tools/testfiles
    tfamily00000.h5
    tfamily00001.h5
    tfamily00002.h5
    tfamily00003.h5
    tfamily00004.h5
    tfamily00005.h5
    tfamily00006.h5
    tfamily00007.h5
    tfamily00008.h5
    tfamily00009.h5
    tfamily00010.h5
    tordergr.h5
    # reference conversion files
    tattrreg.h5
    tdatareg.h5
    # tools/testfiles onion VFD files
    tst_onion_dset_1d.h5
    tst_onion_dset_1d.h5.onion
    tst_onion_dset_ext.h5
    tst_onion_dset_ext.h5.onion
    tst_onion_objs.h5
    tst_onion_objs.h5.onion
)

set (LIST_OTHER_TEST_FILES
    h5repack_ext.bin
    h5repack.info
    ublock.bin
)

set (LIST_TST_TEST_FILES
    h5repack-help.txt
    h5repack_filters.h5-gzip_verbose_filters.tst
    h5repack_layout.h5-dset2_chunk_20x10-errstk.tst
    plugin_test.h5repack_layout.h5.tst
    plugin_version_test.h5repack_layout.h5.tst
    plugin_zero.h5repack_layout.h5.tst
    plugin_none.h5repack_layout.UD.h5.tst
    # tools/testfiles for external links
    tsoftlinks-merge.tsoftlinks.h5.tst
    textlinkfar-merge.textlinkfar.h5.tst
    textlinksrc-merge.textlinksrc.h5.tst
    textlinktar-merge.textlinktar.h5.tst
    textlink-merge.textlink.h5.tst
    h5copy_extlinks_src-merge.h5copy_extlinks_src.h5.tst
)

set (LIST_DDL_TEST_FILES
    crtorder.tordergr.h5.ddl
    deflate_limit.h5repack_layout.h5.ddl
    h5repack_layout.h5.ddl
    h5repack_layout.h5-plugin_test.ddl
    h5repack_layout.h5-plugin_version_test.ddl
    h5repack_layout.h5-plugin_zero.ddl
    h5repack_layout.UD.h5-plugin_none.ddl
    # fsm
    STG.h5repack_none.h5.ddl
    SPT.h5repack_aggr.h5.ddl
    SP.h5repack_fsm_aggr_nopersist.h5.ddl
    S.h5repack_fsm_aggr_persist.h5.ddl
    GS.h5repack_paged_nopersist.h5.ddl
    SP.h5repack_paged_persist.h5.ddl
    # vds
    1_vds.h5-vds_dset_chunk20x10x5-v.ddl
    2_vds.h5-vds_chunk3x6x9-v.ddl
    3_1_vds.h5-vds_chunk2x5x8-v.ddl
    4_vds.h5-vds_compa-v.ddl
    4_vds.h5-vds_conti-v.ddl
    # refs
    attrregion.tattrreg.h5.ddl
    dataregion.tdatareg.h5.ddl
    # tools/testfiles for external links
    textlinkfar-base.textlinkfar.h5.ddl
    textlinksrc-base.textlinksrc.h5.ddl
    textlinktar-base.textlinktar.h5.ddl
    textlink-base.textlink.h5.ddl
    tsoftlinks-base.tsoftlinks.h5.ddl
    h5copy_extlinks_src-base.h5copy_extlinks_src.h5.ddl
    textlinkfar-prune.textlinkfar.h5.ddl
    textlinksrc-prune.textlinksrc.h5.ddl
    textlinktar-prune.textlinktar.h5.ddl
    textlink-prune.textlink.h5.ddl
    tsoftlinks-prune.tsoftlinks.h5.ddl
    h5copy_extlinks_src-prune.h5copy_extlinks_src.h5.ddl
    textlinkfar-mergeprune.textlinkfar.h5.ddl
    textlinksrc-mergeprune.textlinksrc.h5.ddl
    textlinktar-mergeprune.textlinktar.h5.ddl
    textlink-mergeprune.textlink.h5.ddl
    tsoftlinks-mergeprune.tsoftlinks.h5.ddl
    h5copy_extlinks_src-mergeprune.h5copy_extlinks_src.h5.ddl
)

foreach (h5_file ${LIST_REPACK_HDF5_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach(h5_file ${LIST_REPACK_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_COPY_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_DIFF_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_VDS_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/vds/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_HDF5_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_OTHER_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_TST_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/expected/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()

foreach (h5_file ${LIST_DDL_TEST_FILES})
  HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/expected/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
endforeach ()
add_custom_target (h5repack_files ALL COMMENT "Copying files needed by h5repack tests" DEPENDS ${h5repack_files_list})

# Generate testfiles for VOL connector(s) through script
set (h5repack_vol_files_list "")

foreach (external_vol_tgt ${HDF5_EXTERNAL_VOL_TARGETS})
  HDF5_GET_VOL_TGT_INFO (${external_vol_tgt} vol vol_env)

  # Setup testfiles directory
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vol}/testfiles" RESULT)
  if (NOT ${RESULT} EQUAL 0)
    message (FATAL_ERROR "Could not create directory ${PROJECT_BINARY_DIR}/${vol}/testfiles")
  endif()

  add_test (NAME ${external_vol_tgt}-h5repackgentest COMMAND $<TARGET_FILE:h5gentest> --h5repack)

  set_tests_properties (${external_vol_tgt}-h5repackgentest PROPERTIES
      ENVIRONMENT "${vol_env}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/${vol}/testfiles"
      FIXTURES_SETUP h5repack_vol_files
  )

  # These aren't HDF5 files, just copy them to the VOL's subdirectory
  foreach (test_file ${LIST_REPACK_TEST_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/testfiles/${test_file}" "${PROJECT_BINARY_DIR}/${vol}/testfiles/${test_file}" "h5repack_vol_files")
  endforeach ()

  foreach (test_file ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/testfiles/${test_file}" "${PROJECT_BINARY_DIR}/${vol}/testfiles/${test_file}" "h5repack_vol_files")
  endforeach ()

  foreach (test_file ${LIST_TST_TEST_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/expected/${test_file}" "${PROJECT_BINARY_DIR}/${vol}/testfiles/${test_file}" "h5repack_vol_files")
  endforeach ()

  foreach (test_file ${LIST_DDL_TEST_FILES})
    HDFTEST_COPY_FILE ("${HDF5_TOOLS_TST_DIR}/h5repack/expected/${test_file}" "${PROJECT_BINARY_DIR}/${vol}/testfiles/${test_file}" "h5repack_vol_files")
  endforeach ()
endforeach ()

add_custom_target (h5repack_vol_files ALL COMMENT "Copying files needed by h5repack tests" DEPENDS ${h5repack_vol_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

macro (ADD_HELP_TEST testname resultcode)
  # If using memchecker add tests without using scripts
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5REPACK-h5repack-${testname} COMMAND $<TARGET_FILE:h5repack> ${ARGN})
  else ()
    add_test (
        NAME H5REPACK-h5repack-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=h5repack-${testname}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=h5repack-${testname}.txt"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5REPACK-h5repack-${testname} PROPERTIES
      ENVIRONMENT "${CROSSCOMPILING_PATH}"
      WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  if ("H5REPACK-h5repack-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (H5REPACK-h5repack-${testname} PROPERTIES DISABLED true)
  endif ()
endmacro ()

#
# Adds a test that performs h5repack and checks the new file according to passed parameters
# 
# REQUIRED KEYWORDS ARGUMENTS:
#   TEST_TYPE <testtype> - "TEST", "SKIP", or "LEGACY"
#   TEST_FILE <filename> - the target file to use h5repack on
# 
# OPTIONAL KEYWORD ARGUMENTS:
#   RESULT_CODE <code> - expected return code from h5repack (default 0)
#   DIFF_RESULT_CODE <code> - expected return code from h5diff (default 0)
#   ERR_REF <string> - value for TEST_ERRREF (default empty)
#   STAT_ARG <arg> - argument to pass to h5stat (required if STAT_CHECK is provided)
#   STAT_RESULT_CODE <code> - expected return code from h5stat (default 0)
#   LAYOUT_DSET <dset> - dataset to use for layout verification tests
#   LAYOUT_FILTER <filter> - filter to use for layout verification tests
#
# OPTIONAL FLAGS
#   ERROR_STACK - pass the '--enable-error-stack' flag to h5repack and h5diff
#   GZIP_FILTER - Apply filters to genericize gzip-related output for comparison
#   SIZE_FILTER - Apply filters to genericize size-related output for comparison
#   DUMP_CHECK - Whether to use h5dump to verify output
#   DUMP_NO_OPT - Whether to provide additional cmd line arguments to h5dump
#                 No effect if DUMP_CHECK is not provided
#   FULL_DIFF - Whether to perform h5diff verification through runTest
#   STAT_CHECK - Whether to use h5stat to verify output
#   NATIVE_ONLY - Whether to only run the test with the native VOL connector
#
macro (ADD_H5_TEST testname)
  # === Argument handling ===
  cmake_parse_arguments (ARG
      "ERROR_STACK;GZIP_FILTER;SIZE_FILTER;DUMP_CHECK;DUMP_NO_OPT;FULL_DIFF;STAT_CHECK;NATIVE_ONLY" # flags
      "TEST_TYPE;TEST_FILE;RESULT_CODE;DIFF_RESULT_CODE;ERR_REF;STAT_ARG;STAT_RESULT_CODE;LAYOUT_DSET;LAYOUT_FILTER" # single arg
      "" # multi arg
      ${ARGN}
  )

  # Check for required arguments
  if (NOT ARG_TEST_TYPE)
    message (FATAL_ERROR "ADD_H5_TEST: TEST_TYPE is a required argument")
  endif ()

  if (NOT ARG_TEST_FILE)
    message (FATAL_ERROR "ADD_H5_TEST: TEST_FILE is a required argument")
  endif ()

  # Default values for local variables
  set (ARG_CLEANUP_DEPENDS "")

  set (base_testname ${testname})

  set (ARG_FILTER_IN "")
  set (ARG_FILTER_OUT "")
  set (ARG_REF_FILE "")

  set (ARG_DUMP_SKIP_COMPARE FALSE)
  set (ARG_DUMP_OPTIONS "-q;creation_order;-pH;")
  set (ARG_NOT_LAYOUT_FILTER "")
  set (ARG_DUMP_GREP_FILTER "")
  set (ARG_DUMP_REFERENCE "${testname}.${ARG_TEST_FILE}.ddl")

  set (ARG_STDOUT_FILE "${ARG_TEST_FILE}-${testname}.out")
  set (ARG_ERROR_STACK_FLAG "")

  # Whether to perform comparison in runTest or locally with h5diff
  set (ARG_COMPARE_LOCAL true)

  if (${ARG_STAT_CHECK})
    if (NOT DEFINED ARG_STAT_ARG)
      message (FATAL_ERROR "ADD_H5_TEST: STAT_ARG is a required argument when STAT_CHECK is provided")
    endif ()

    set (ARG_MAIN_OUT_FILE "out-${ARG_STAT_ARG}.${ARG_TEST_FILE}")
  else ()
    set (ARG_MAIN_OUT_FILE "out-${testname}.${ARG_TEST_FILE}")
  endif ()

  # Process optional arguments
  if (ARG_ERROR_STACK)
    set (ARG_ERROR_STACK_FLAG "--enable-error-stack")
  endif ()

  if (NOT DEFINED ARG_RESULT_CODE)
    set (ARG_RESULT_CODE 0)
  endif ()

  if (NOT DEFINED ARG_DIFF_RESULT_CODE)
    set (ARG_DIFF_RESULT_CODE 0)
  endif ()

  if (NOT DEFINED ARG_STAT_RESULT_CODE)
    set (ARG_STAT_RESULT_CODE 0)
  endif ()

  if (ARG_DUMP_NO_OPT)
    set (ARG_DUMP_OPTIONS "")
  endif()

  if (DEFINED ARG_LAYOUT_DSET OR DEFINED ARG_LAYOUT_FILTER)
    if (NOT DEFINED ARG_LAYOUT_DSET OR NOT DEFINED ARG_LAYOUT_FILTER)
      message (FATAL_ERROR "ADD_H5_TEST: Both LAYOUT_DSET and LAYOUT_FILTER must be provided for layout verification tests")
    endif ()
    
    if (${ARG_DUMP_CHECK})
      message (FATAL_ERROR "ADD_H5_TEST: DUMP_CHECK cannot be used with layout verification tests")
    endif ()

    # Skip verify layout tests if memchecker enabled
    if (HDF5_ENABLE_USING_MEMCHECKER)
      set (ARG_TEST_TYPE "SKIP")
    endif ()

    set (base_testname "VERIFY_LAYOUT-${testname}")
    set (ARG_DUMP_SKIP_COMPARE TRUE)
    set (ARG_STDOUT_FILE "${ARG_TEST_FILE}-${testname}-v.out")
    set (ARG_DUMP_REFERENCE "${ARG_LAYOUT_FILTER}")
    # If an error result code is provided, change the h5dump call in the following ways
    # - remove use of dset in args
    # - change expected result code to 0
    # - change the layout filter provided to TEST_GREP_FILTER
    if (${ARG_RESULT_CODE})
      set (ARG_DUMP_OPTIONS "-pH;")

      if (${ARG_LAYOUT_FILTER} STREQUAL "CHUNKED")
        set (ARG_NOT_LAYOUT_FILTER "(CONTIGUOUS|COMPACT)")
      elseif (${ARG_LAYOUT_FILTER} STREQUAL "CONTIGUOUS")
        set (ARG_NOT_LAYOUT_FILTER "(CHUNK|COMPACT)")
      elseif (${ARG_LAYOUT_FILTER} STREQUAL "COMPACT")
        set (ARG_NOT_LAYOUT_FILTER "(CONTIGUOUS|CHUNK)")
      else ()
        message (FATAL_ERROR "ADD_H5_TEST: Invalid LAYOUT_FILTER value ${ARG_LAYOUT_FILTER}. Must be CONTIGUOUS, CHUNKED, or COMPACT")
      endif ()

      set (ARG_DUMP_GREP_FILTER ${ARG_NOT_LAYOUT_FILTER})
      set (ARG_RESULT_CODE 0)
    else ()
      set (ARG_DUMP_OPTIONS "-d;${ARG_LAYOUT_DSET};-pH;")
      set (ARG_DUMP_GREP_FILTER ${ARG_LAYOUT_FILTER})
    endif ()
  endif ()

  # Check for incompatible options
  if (${ARG_GZIP_FILTER} AND DEFINED ARG_ERR_REF)
    message (FATAL_ERROR "ADD_H5_TEST: GZIP_FILTER and ERR_REF options are incompatible")
  endif ()
  if (${ARG_FULL_DIFF} AND ${ARG_DUMP_CHECK})
    message (FATAL_ERROR "ADD_H5_TEST: FULL_DIFF and DUMP_CHECK options are incompatible")
  endif ()
  if (${ARG_STAT_CHECK} AND ${ARG_DUMP_CHECK})
    message (FATAL_ERROR "ADD_H5_TEST: STAT_CHECK and DUMP_CHECK options are incompatible")
  endif ()

  # not fundamentally incompatible, but at present this probably indicates a mistake in test setup
  if (${ARG_GZIP_FILTER} AND ${ARG_SIZE_FILTER})
    message (FATAL_ERROR "ADD_H5_TEST: GZIP_FILTER and SIZE_FILTER options are incompatible")
  endif ()

  if (ARG_GZIP_FILTER)
    list (APPEND ARG_FILTER_IN "GZIP   \\(0\\.[0-9][0-9][0-9]:1\\);O?...ing file[^\n]+\n")
    list (APPEND ARG_FILTER_OUT "GZIP   (0.XXX:1);")
    set (ARG_REF_FILE "${ARG_TEST_FILE}-${testname}.tst")
    set (base_testname "CMP-${base_testname}")
    # Don't skip runTest comparison if we're using filters
    set (ARG_COMPARE_LOCAL false)
  elseif (ARG_SIZE_FILTER)
    list (APPEND ARG_FILTER_IN "SIZE [0-9][0-9][0-9][0-9] \\(2\\\.[0-9][0-9][0-9]:1 COMPRESSION\\)")
    list (APPEND ARG_FILTER_OUT "SIZE XXXX (2.XXX:1 COMPRESSION)")
    # Don't skip runTest comparison if we're using filters
    set (ARG_COMPARE_LOCAL false)
  endif ()

  # Size/Offset within file will differ across VOL connectors
  # These are inserted to the list now to be applied after compression-related output masks
  list (APPEND ARG_FILTER_IN "OFFSET [0-9]+")
  list (APPEND ARG_FILTER_OUT "OFFSET XXXX")

  list (APPEND ARG_FILTER_IN "SIZE [0-9]+")
  list (APPEND ARG_FILTER_OUT "SIZE XXXX")

  if (${ARG_DUMP_CHECK})
    set (ARG_COMPARE_LOCAL false)
  endif ()

  if (${ARG_STAT_CHECK})
    set (ARG_COMPARE_LOCAL false)
  endif ()

  if (${ARG_NATIVE_ONLY})
    set (num_ext_vols 0)
  else ()
    list (LENGTH HDF5_EXTERNAL_VOL_TARGETS num_ext_vols)
  endif ()

  # Add a test for the native connector and each external VOL connector
  foreach (vol_idx RANGE 0 ${num_ext_vols})
    # First, populate VOL info to be passed to tests
    if (${vol_idx} EQUAL 0)
      set (vol "native")
      set (vol_env "${CROSSCOMPILING_PATH}")
      set (vol_prefix "")
      # Avoid names of the form XXX_BINARY_DIR due to collision risk
      set (BINARY_DIR_VOL "${PROJECT_BINARY_DIR}")
    else ()
      # An external VOL connector
      set (vol_env "")

      math (EXPR vol_idx_fixed "${vol_idx} - 1")
      list (GET HDF5_EXTERNAL_VOL_TARGETS ${vol_idx_fixed} ext_vol_tgt)
      HDF5_GET_VOL_TGT_INFO (${ext_vol_tgt} vol vol_env)

      set (vol_prefix "HDF5_VOL_${vol}-")
      # Avoid names of the form XXX_BINARY_DIR due to collision risk
      set (BINARY_DIR_VOL "${PROJECT_BINARY_DIR}/${vol}")
    endif ()

    # === Set up names for the distinct generated tests ===
    set (REPACK_TESTNAME "${vol_prefix}H5REPACK-${base_testname}")

    set (CLEAR_TESTNAME  "${REPACK_TESTNAME}-clear-objects")
    set (CLEAN_TESTNAME  "${REPACK_TESTNAME}-clean-objects")

    set (DIFF_TESTNAME   "${REPACK_TESTNAME}-h5diff")
    set (DUMP_TESTNAME   "${REPACK_TESTNAME}-h5dump")
    set (STAT_TESTNAME   "${REPACK_TESTNAME}-h5stat")

    # === Create test ===
    if ("${ARG_TEST_TYPE}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME ${REPACK_TESTNAME}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARG_UNPARSED_ARGUMENTS} -i ${BINARY_DIR_VOL}/testfiles/${ARG_TEST_FILE} -o ${BINARY_DIR_VOL}/testfiles/${ARG_MAIN_OUT_FILE}"
        )
        set_property (TEST ${REPACK_TESTNAME} PROPERTY DISABLED true)
      endif ()
    else ()
      # Test is to be run
      add_test (
          NAME ${CLEAR_TESTNAME}
          COMMAND ${CMAKE_COMMAND} -E remove "${BINARY_DIR_VOL}/testfiles/${ARG_MAIN_OUT_FILE}"
      )

      set_tests_properties (${CLEAR_TESTNAME} PROPERTIES
          WORKING_DIRECTORY "${BINARY_DIR_VOL}/testfiles"
          FIXTURES_REQUIRED h5repack_vol_files
      )

      # Build REPACK_TOOL_ARGS properly to avoid empty first argument
      set (REPACK_TOOL_ARGS "")

      if (ARG_ERROR_STACK_FLAG)
        list (APPEND REPACK_TOOL_ARGS "${ARG_ERROR_STACK_FLAG}")
      endif ()

      if (ARG_UNPARSED_ARGUMENTS)
        list (APPEND REPACK_TOOL_ARGS "${ARG_UNPARSED_ARGUMENTS}")
      endif ()

      if (HDF5_ENABLE_USING_MEMCHECKER OR ${ARG_DUMP_CHECK})
        # Execute h5repack directly - append absolute paths
        list (APPEND REPACK_TOOL_ARGS "-i;${BINARY_DIR_VOL}/testfiles/${ARG_TEST_FILE};-o;${BINARY_DIR_VOL}/testfiles/${ARG_MAIN_OUT_FILE}")

        add_test (
            NAME ${REPACK_TESTNAME}
            COMMAND $<TARGET_FILE:h5repack> ${REPACK_TOOL_ARGS}
        )

        if (NOT "${vol}" STREQUAL "native")
          set_tests_properties (${REPACK_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
              WORKING_DIRECTORY "${BINARY_DIR_VOL}/testfiles"
              FIXTURES_REQUIRED h5repack_vol_files
          )
        else ()
          set_tests_properties (${REPACK_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
          )
        endif ()

      else ()
        # Execute h5repack through runTest script - append relative paths
        list (APPEND REPACK_TOOL_ARGS "-i;${ARG_TEST_FILE};-o;${ARG_MAIN_OUT_FILE}")

        add_test (
            NAME ${REPACK_TESTNAME}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
                -D "TEST_ARGS:STRING=${REPACK_TOOL_ARGS}"
                -D "TEST_FOLDER=${BINARY_DIR_VOL}/testfiles"
                -D "TEST_OUTPUT=${ARG_STDOUT_FILE}"
                -D "TEST_EXPECT=${ARG_RESULT_CODE}"
                -D "TEST_SKIP_COMPARE=${ARG_COMPARE_LOCAL}"
                -D "TEST_FILTER:STRING=${ARG_FILTER_IN}"
                -D "TEST_FILTER_REPLACE:STRING=${ARG_FILTER_OUT}"
                -D "TEST_REFERENCE=${ARG_REF_FILE}"
                -D "TEST_ERRREF=${ARG_ERR_REF}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )

        if (NOT "${vol}" STREQUAL "native")
          set_tests_properties (${REPACK_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
              # working directory handled by runTest
              FIXTURES_REQUIRED h5repack_vol_files
          )
        else ()
          set_tests_properties (${REPACK_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
          )
        endif ()
      endif ()

      set_tests_properties (${REPACK_TESTNAME} PROPERTIES
          DEPENDS ${CLEAR_TESTNAME}
      )

      list (APPEND ARG_CLEANUP_DEPENDS "${REPACK_TESTNAME}")

      if ("${REPACK_TESTNAME}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (${REPACK_TESTNAME} PROPERTIES DISABLED true)
      endif ()

      # If we're doing local comparison, add h5diff test
      if ((${ARG_COMPARE_LOCAL} AND NOT ${ARG_FULL_DIFF}) OR DEFINED ARG_LAYOUT_DSET)
        # h5diff directly
        add_test (
            NAME ${DIFF_TESTNAME}
            COMMAND $<TARGET_FILE:h5diff> ${ARG_ERROR_STACK_FLAG} ${BINARY_DIR_VOL}/testfiles/${ARG_TEST_FILE} ${BINARY_DIR_VOL}/testfiles/${ARG_MAIN_OUT_FILE}
        )
        set_tests_properties (${DIFF_TESTNAME} PROPERTIES
            DEPENDS ${REPACK_TESTNAME}
            ENVIRONMENT "${CROSSCOMPILING_PATH}"
            WORKING_DIRECTORY "${BINARY_DIR_VOL}"
        )
        if ("${DIFF_TESTNAME}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (${DIFF_TESTNAME} PROPERTIES DISABLED true)
        endif ()
      elseif (${ARG_COMPARE_LOCAL} AND ${ARG_FULL_DIFF})
        # h5diff via runTest
        add_test (
            NAME ${DIFF_TESTNAME}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
                -D "TEST_ARGS:STRING=-v;${ARG_ERROR_STACK_FLAG};${BINARY_DIR_VOL}/testfiles/${ARG_TEST_FILE};${BINARY_DIR_VOL}/testfiles/${ARG_MAIN_OUT_FILE}"
                -D "TEST_FOLDER=${BINARY_DIR_VOL}/testfiles"
                -D "TEST_OUTPUT=${ARG_MAIN_OUT_FILE}.out"
                -D "TEST_EXPECT=${ARG_DIFF_RESULT_CODE}"
                -D "TEST_REFERENCE=${testname}.${ARG_TEST_FILE}.tst"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (${DIFF_TESTNAME} PROPERTIES
            DEPENDS ${REPACK_TESTNAME}
        )
        if (NOT "${vol}" STREQUAL "native")
          set_tests_properties (${DIFF_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
              # working directory handled by runTest
              FIXTURES_REQUIRED h5repack_vol_files
          )
        else ()
          set_tests_properties (${DIFF_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
          )
        endif ()
        if ("${DIFF_TESTNAME}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (${DIFF_TESTNAME} PROPERTIES DISABLED true)
        endif ()

      endif ()

      list (APPEND ARG_CLEANUP_DEPENDS "${DIFF_TESTNAME}")

      if (${ARG_DUMP_CHECK} OR DEFINED ARG_LAYOUT_DSET)
        # Perform check via h5dump
        add_test (
            NAME ${DUMP_TESTNAME}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=${ARG_DUMP_OPTIONS};${ARG_MAIN_OUT_FILE}"
                -D "TEST_FOLDER=${BINARY_DIR_VOL}/testfiles"
                -D "TEST_OUTPUT=${ARG_STDOUT_FILE}"
                -D "TEST_EXPECT=${ARG_RESULT_CODE}"
                -D "TEST_SKIP_COMPARE=${ARG_DUMP_SKIP_COMPARE}"
                -D "TEST_GREP_FILTER=${ARG_DUMP_GREP_FILTER}"
                -D "TEST_GREP_EXPECT=${ARG_RESULT_CODE}"
                -D "TEST_FILTER:STRING=${ARG_FILTER_IN}"
                -D "TEST_FILTER_REPLACE:STRING=${ARG_FILTER_OUT}"
                -D "TEST_REFERENCE=${ARG_DUMP_REFERENCE}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        if (NOT "${vol}" STREQUAL "native")
          set_tests_properties (${DUMP_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
              # working directory handled by runTest
              FIXTURES_REQUIRED h5repack_vol_files
          )
        else ()
          set_tests_properties (${DUMP_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
          )
        endif ()

        set_tests_properties (${DUMP_TESTNAME} PROPERTIES
            DEPENDS ${REPACK_TESTNAME}
        )

        if ("${DUMP_TESTNAME}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (${DUMP_TESTNAME} PROPERTIES DISABLED true)
        endif ()

        list (APPEND ARG_CLEANUP_DEPENDS "${DUMP_TESTNAME}")
      endif ()

      if (${ARG_STAT_CHECK} AND NOT HDF5_ENABLE_USING_MEMCHECKER)
        add_test (
            NAME ${STAT_TESTNAME}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
                -D "TEST_ARGS:STRING=-S;-s;out-${ARG_STAT_ARG}.${ARG_TEST_FILE}"
                -D "TEST_FOLDER=${BINARY_DIR_VOL}/testfiles"
                -D "TEST_OUTPUT=${ARG_STDOUT_FILE}"
                -D "TEST_EXPECT=${ARG_STAT_RESULT_CODE}"
                -D "TEST_REFERENCE=${ARG_STAT_ARG}.${ARG_TEST_FILE}.ddl"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        if (NOT "${vol}" STREQUAL "native")
          set_tests_properties (${STAT_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
              # working directory handled by runTest
              FIXTURES_REQUIRED h5repack_vol_files
          )
        else ()
          set_tests_properties (${STAT_TESTNAME} PROPERTIES
              ENVIRONMENT "${vol_env}"
          )
        endif ()

        set_tests_properties (${STAT_TESTNAME} PROPERTIES
            DEPENDS ${REPACK_TESTNAME}
        )
        if ("${STAT_TESTNAME}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties ("${STAT_TESTNAME}" PROPERTIES DISABLED true)
        endif ()

        list (APPEND ARG_CLEANUP_DEPENDS "${STAT_TESTNAME}")
      endif ()

      # Post-test cleanup
      add_test (
          NAME ${CLEAN_TESTNAME}
          COMMAND ${CMAKE_COMMAND} -E remove "${BINARY_DIR_VOL}/testfiles/${ARG_MAIN_OUT_FILE}"
      )
      set_tests_properties (${CLEAN_TESTNAME} PROPERTIES
          WORKING_DIRECTORY "${BINARY_DIR_VOL}/testfiles"
          FIXTURES_REQUIRED h5repack_vol_files
      )
      # Only set dependencies if necessary
      if (NOT "${ARG_CLEANUP_DEPENDS}" STREQUAL "")
        set_tests_properties (${CLEAN_TESTNAME} PROPERTIES
            DEPENDS "${ARG_CLEANUP_DEPENDS}"
        )
      else ()
        set_tests_properties (${CLEAN_TESTNAME} PROPERTIES
            ENVIRONMENT "${vol_env}"
        )
      endif ()
    endif ()
  endforeach () # per-VOL loop
endmacro ()

macro (ADD_H5_VERIFY_VDS testname testtype resultcode testfile testdset testfilter)
  if ("${testtype}" STREQUAL "SKIP")
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP -d ${testdset} -pH ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
      )
      set_property (TEST H5REPACK_VERIFY_LAYOUT_VDS-${testname} PROPERTY DISABLED true)
    endif ()
  else ()
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      add_test (
          NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}
          COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname} PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clear-objects
      )
      if ("H5REPACK_VERIFY_LAYOUT_VDS-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=-d;${testdset};-p;out-${testname}.${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${testfile}-${testname}-v.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_VERIFY_LAYOUT_VDS-${testname}
      )
      if ("H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP
      )
    endif ()
  endif ()
endmacro ()

# VERIFY_SUPERBLOCK
macro (ADD_H5_VERIFY_SUPERBLOCK testname testfile lowbound highbound superblock)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
    )
    add_test (
        NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}
        COMMAND $<TARGET_FILE:h5repack> -j;${lowbound};-k;${highbound} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
    )
    set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_VERIFY_SUPERBLOCK-${testname}-clear-objects
    )
    if ("H5REPACK_VERIFY_SUPERBLOCK-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=-H;-B;out-${testname}.${testfile}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_GREP_FILTER:STRING=SUPERBLOCK_VERSION ${superblock}"
            -D "TEST_REFERENCE=SUPERBLOCK_VERSION ${superblock}"
            -D "TEST_GREP_COMPARE=TRUE"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_VERIFY_SUPERBLOCK-${testname}
    )
    if ("H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
    )
    set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname}-clean-objects PROPERTIES
        DEPENDS H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP
    )
  endif ()
endmacro ()

macro (ADD_H5_VERIFY_INVALIDBOUNDS testname resultcode lowbound highbound)
    add_test (
        NAME ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
    )
    add_test (
        NAME ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}
        COMMAND $<TARGET_FILE:h5repack> -j;${lowbound};-k;${highbound} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
    )
    set_tests_properties (ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clear-objects
        WILL_FAIL "true"
    )
    if ("ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
    )
    set_tests_properties (ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clean-objects PROPERTIES
        DEPENDS ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}
    )
endmacro ()

macro (ADD_H5_VERIFY_USERBLOCK testname userblocksize testfile)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5REPACK_VERIFY_USERBLOCK-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
    )
    add_test (
        NAME H5REPACK_VERIFY_USERBLOCK-${testname}
        COMMAND $<TARGET_FILE:h5repack> --enable-error-stack ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
    )
    set_tests_properties (H5REPACK_VERIFY_USERBLOCK-${testname} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_VERIFY_USERBLOCK-${testname}-clear-objects
    )
    if ("H5REPACK_VERIFY_USERBLOCK-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_VERIFY_USERBLOCK-${testname} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_VERIFY_USERBLOCK-${testname}_DMP
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=-H;-B;out-${testname}.${testfile}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_GREP_FILTER:STRING=USERBLOCK_SIZE ${userblocksize}"
            -D "TEST_REFERENCE=USERBLOCK_SIZE ${userblocksize}"
            -D "TEST_GREP_COMPARE=TRUE"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5REPACK_VERIFY_USERBLOCK-${testname}_DMP PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_VERIFY_USERBLOCK-${testname}
    )
    if ("H5REPACK_VERIFY_USERBLOCK-${testname}_DMP" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_VERIFY_USERBLOCK-${testname}_DMP PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_VERIFY_USERBLOCK-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
    )
    set_tests_properties (H5REPACK_VERIFY_USERBLOCK-${testname}-clean-objects PROPERTIES
        DEPENDS H5REPACK_VERIFY_USERBLOCK-${testname}_DMP
    )
  endif ()
endmacro ()

macro (ADD_H5_TEST_META testname testfile)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5REPACK_META-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            testfiles/out-${testname}_N.${testname}.h5
            testfiles/out-${testname}_M.${testname}.h5
    )
    add_test (
        NAME H5REPACK_META-${testname}_N
        COMMAND $<TARGET_FILE:h5repack> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_N.${testname}.h5
    )
    set_tests_properties (H5REPACK_META-${testname}_N PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_META-${testname}-clear-objects
    )
    if ("H5REPACK_META-${testname}_N" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_META-${testname}_N PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_META-${testname}_N_DFF
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
            -D "TEST_ARGS:STRING=-v;${testfile};out-${testname}_N.${testname}.h5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=out-${testname}_N.${testname}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_REFERENCE=out-${testname}_N.${testname}.txt"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5REPACK_META-${testname}_N_DFF PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_META-${testname}_N
    )
    if ("H5REPACK_META-${testname}_N_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_META-${testname}_N_DFF PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_META-${testname}_M
        COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_M.${testname}.h5
    )
    set_tests_properties (H5REPACK_META-${testname}_M PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_META-${testname}_N_DFF
    )
    if ("H5REPACK_META-${testname}_M" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_META-${testname}_M PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_META-${testname}_M_DFF
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
            -D "TEST_ARGS:STRING=-v;${testfile};out-${testname}_M.${testname}.h5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_OUTPUT=out-${testname}_M.${testname}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_REFERENCE=out-${testname}_M.${testname}.txt"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5REPACK_META-${testname}_M_DFF PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_META-${testname}_M
    )
    if ("H5REPACK_META-${testname}_M_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_META-${testname}_M_DFF PROPERTIES DISABLED true)
    endif ()
    add_test (NAME H5REPACK_META-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_ONEFILE=out-${testname}_N.${testname}.h5"
            -D "TEST_TWOFILE=out-${testname}_M.${testname}.h5"
            -D "TEST_FUNCTION=LTEQ"
            -P "${HDF_RESOURCES_DIR}/fileCompareTest.cmake"
    )
    set_tests_properties (H5REPACK_META-${testname} PROPERTIES
        DEPENDS H5REPACK_META-${testname}_M_DFF
    )
    if ("H5REPACK_META-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_META-${testname} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5REPACK_META-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            testfiles/out-${testname}_N.${testname}.h5
            testfiles/out-${testname}_M.${testname}.h5
    )
    set_tests_properties (H5REPACK_META-${testname}-clean-objects PROPERTIES
        DEPENDS H5REPACK_META-${testname}
    )
endmacro ()

macro (ADD_H5_UD_TEST testname resultcode resultfile)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5REPACK_UD-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
    )
    add_test (
        NAME H5REPACK_UD-${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
            -D "TEST_ARGS:STRING=${ARGN};${resultfile};out-${testname}.${resultfile}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_FILTER:STRING=O?...ing file[^\n]+\n"
            -D "TEST_OUTPUT=${testname}.${resultfile}.out"
            -D "TEST_REFERENCE=${testname}.${resultfile}.tst"
            -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
            -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (H5REPACK_UD-${testname} PROPERTIES
        ENVIRONMENT "${CROSSCOMPILING_PATH}"
        DEPENDS H5REPACK_UD-${testname}-clear-objects
    )
    if ("H5REPACK_UD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK_UD-${testname} PROPERTIES DISABLED true)
    endif ()
    if (NOT ${resultcode})
      add_test (
          NAME H5REPACK_UD-${testname}-h5dump
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=-pH;out-${testname}.${resultfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}-${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${resultfile}-${testname}.ddl"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_UD-${testname}-h5dump PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          DEPENDS H5REPACK_UD-${testname}
      )
      if ("H5REPACK_UD-${testname}-h5dump" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_UD-${testname}-h5dump PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_UD-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_UD-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_UD-${testname}-h5dump
      )
    else ()
      add_test (
          NAME H5REPACK_UD-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_UD-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_UD-${testname}
      )
    endif ()
  endif ()
endmacro ()

macro (ADD_H5_EXTERNAL_TEST testname testtype testfile)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    # canonical file = h5repack_${testfile}.h5 - preexist
    # external file = h5repack_${testfile}_ex.h5 - preexist
    # repacked file = h5repack_${testfile}_rp.h5 - created
    # external data file = h5repack_${testfile}_ex-0.dat
    if ("${testtype}" STREQUAL "SKIP")
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
      )
      set_property(TEST H5REPACK_EXTERNAL-${testname} PROPERTY DISABLED true)
    else ()
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove h5repack_${testfile}_rp.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
      )
      # make sure external data file 0 is available
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_CPY
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/h5repack_${testfile}_ex-0.dat" "${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex-0.dat"
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_CPY PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}-clear-objects
      )
      if ("H5REPACK_EXTERNAL-${testname}_CPY" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_CPY PROPERTIES DISABLED true)
      endif ()
      # comparison of known files
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_DFF1
          COMMAND $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF1 PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_CPY
      )
      if ("H5REPACK_EXTERNAL-${testname}_DFF1" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF1 PROPERTIES DISABLED true)
      endif ()
      # repack the external file to the repacked file
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}
          COMMAND $<TARGET_FILE:h5repack> --enable-error-stack ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname} PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_DFF1
      )
      if ("H5REPACK_EXTERNAL-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname} PROPERTIES DISABLED true)
      endif ()
      # comparison of repacked file to known files
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_DFF2
          COMMAND $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF2 PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}
      )
      if ("H5REPACK_EXTERNAL-${testname}_DFF2" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF2 PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_DFF3
          COMMAND $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF3 PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_DFF2
      )
      if ("H5REPACK_EXTERNAL-${testname}_DFF3" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF3 PROPERTIES DISABLED true)
      endif ()
      # invalidate external file by removing its first data file
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_DATA_RMV
          COMMAND ${CMAKE_COMMAND} -E remove h5repack_${testfile}_ex-0.dat
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_DATA_RMV PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_DFF3
      )
      if ("H5REPACK_EXTERNAL-${testname}_DATA_RMV" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DATA_RMV PROPERTIES DISABLED true)
      endif ()
      # verify comparison of repacked file to known file
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_DFF4
          COMMAND $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF4 PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_DATA_RMV
      )
      if ("H5REPACK_EXTERNAL-${testname}_DFF4" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF4 PROPERTIES DISABLED true)
      endif ()
      # verify comparison of repacked file to known external file fails
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}_DFF_FAIL
          COMMAND $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF_FAIL PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_DFF4
          WILL_FAIL "true"
      )
      if ("H5REPACK_EXTERNAL-${testname}_DFF_FAIL" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF_FAIL PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_EXTERNAL-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove h5repack_${testfile}_rp.h5
      )
      set_tests_properties (H5REPACK_EXTERNAL-${testname}-clean-objects PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5REPACK_EXTERNAL-${testname}_DFF_FAIL
      )
    endif ()
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# test file names
# --------------------------------------------------------------------
set (INFO_FILE h5repack.info)

set (FILE0 h5repack_fill.h5)
set (FILE1 h5repack_objs.h5)
set (FILE2 h5repack_attr.h5)
set (FILE3 h5repack_hlink.h5)
set (FILE4 h5repack_layout.h5)
set (FILE5 h5repack_early.h5)
set (FILE7 h5repack_szip.h5)
set (FILE8 h5repack_deflate.h5)
set (FILE9 h5repack_shuffle.h5)
set (FILE10 h5repack_fletcher.h5)
set (FILE11 h5repack_filters.h5)
set (FILE12 h5repack_nbit.h5)
set (FILE13 h5repack_soffset.h5)
set (FILE14 h5repack_layouto.h5 )     # A file with an older version of the layout message (copy of test/tlayouto.h5)
set (FILE15 h5repack_named_dtypes.h5)
set (FILE16 tfamily%05d.h5)           # located in common testfiles folder
set (FILE18 h5repack_layout2.h5)
set (FILE19 h5repack_layout3.h5)
set (FILE_REF h5repack_refs.h5)
set (FILE_ATTR_REF h5repack_attr_refs.h5)
set (FILEV1 1_vds.h5)
set (FILEV2 2_vds.h5)
set (FILEV3_1 3_1_vds.h5)
set (FILEV3_2 3_2_vds.h5)
set (FILEV4 4_vds.h5)
set (FILEV5 5_vds.h5)

ADD_HELP_TEST(help 0 -h)

add_test (NAME H5REPACK-testh5repack_detect_szip COMMAND $<TARGET_FILE:testh5repack_detect_szip>)
if (HDF5_ENABLE_SZIP_SUPPORT)
  if (HDF5_ENABLE_SZIP_ENCODING)
    set (passRegex "yes")
    set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "yes")
  else ()
    set (passRegex "no")
    set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
  endif ()
else ()
  set (passRegex "no")
  set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
endif ()
set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES
    DEPENDS H5REPACK-h5repack-${testname}
    ENVIRONMENT "${CROSSCOMPILING_PATH}"
)
set (last_test "H5REPACK-testh5repack_detect_szip")

#  add_test (NAME H5REPACK-h5repacktest COMMAND $<TARGET_FILE:h5repacktest>)
#  set_tests_properties (H5REPACK-h5repacktest PROPERTIES
#    DEPENDS H5REPACK-testh5repack_detect_szip
#    ENVIRONMENT "${CROSSCOMPILING_PATH}"
#  )
#  set (last_test "H5REPACK-h5repacktest")
#
# The tests
# We use the files generated by h5repacktst
# Each run generates "<file>.out.h5" and the tool h5diff is used to
# compare the input and output files
#
# the tests are the same as the program h5repacktst, but run from the CLI
#

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
#  set (USE_FILTER_SZIP_ENCODER 0)
if (HDF5_ENABLE_SZIP_ENCODING)
  set (USE_FILTER_SZIP_ENCODER ${testh5repack_detect_szip})
endif ()

if (H5_HAVE_FILTER_DEFLATE)
  set (USE_FILTER_DEFLATE 1)
endif ()

if (H5_HAVE_FILTER_SZIP)
  set (USE_FILTER_SZIP 1)
endif ()

# copy files (these files have no filters)
ADD_H5_TEST (fill TEST_TYPE "TEST" TEST_FILE ${FILE0} ERROR_STACK)
ADD_H5_TEST (objs TEST_TYPE "TEST" TEST_FILE ${FILE1} ERROR_STACK)
ADD_H5_TEST (attr TEST_TYPE "TEST" TEST_FILE ${FILE2} ERROR_STACK)
ADD_H5_TEST (hlink TEST_TYPE "TEST" TEST_FILE ${FILE3} ERROR_STACK)
ADD_H5_TEST (layout TEST_TYPE "TEST" TEST_FILE ${FILE4} ERROR_STACK)
ADD_H5_TEST (early TEST_TYPE "TEST" TEST_FILE ${FILE5} ERROR_STACK)

# nested 8bit enum in both deflated and non-deflated datafiles
if (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (nested_8bit_enum TEST_TYPE "TEST" TEST_FILE h5repack_nested_8bit_enum.h5 NATIVE_ONLY)
else ()
  ADD_H5_TEST (nested_8bit_enum TEST_TYPE "TEST" TEST_FILE h5repack_nested_8bit_enum_deflated.h5 NATIVE_ONLY)
endif ()

# use $FILE4 to write some filters  (this file has  no filters)

# gzip with individual object
set (arg ${FILE4} -f dset1:GZIP=1  -l dset1:CHUNK=20x10)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (gzip_individual TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# gzip for all
set (arg ${FILE4} -f GZIP=1)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (gzip_all TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# szip with individual object
set (arg ${FILE4} -f dset2:SZIP=8,EC  -l dset2:CHUNK=20x10)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (szip_individual TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# szip for all
set (arg ${FILE4} -f SZIP=8,NN)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (szip_all TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# shuffle with individual object
set (arg ${FILE4} -f dset2:SHUF  -l dset2:CHUNK=20x10)
ADD_H5_TEST (shuffle_individual TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# shuffle for all
set (arg ${FILE4} -f SHUF)
ADD_H5_TEST (shuffle_all TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# fletcher32  with individual object
set (arg ${FILE4} -f dset2:FLET  -l dset2:CHUNK=20x10)
ADD_H5_TEST (fletcher_individual TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# fletcher32 for all
set (arg ${FILE4} -f FLET)
ADD_H5_TEST (fletcher_all TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# all filters
set (arg ${FILE4} -f dset2:SHUF -f dset2:FLET -f dset2:SZIP=8,NN -f dset2:GZIP=1 -l dset2:CHUNK=20x10)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (all_filters TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# verbose gzip with individual object
set (arg ${FILE11} -v -f /dset_deflate:GZIP=9)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
# Not fundamentally VOL incompatible, but requires reworking test filter path setup for compatibility
ADD_H5_TEST (gzip_verbose_filters TEST_TYPE ${TESTTYPE} RESULT_CODE 0 TEST_FILE ${arg} GZIP_FILTER NATIVE_ONLY)

###########################################################
# the following tests assume the input files have filters
###########################################################

# szip copy
set (arg ${FILE7})
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (szip_copy TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# szip remove
set (arg ${FILE7} --filter=dset_szip:NONE)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (szip_remove TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# deflate copy
set (arg ${FILE8})
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (deflate_copy TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# deflate remove
set (arg ${FILE8} -f dset_deflate:NONE)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (deflate_remove TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# shuffle copy
set (arg ${FILE9})
ADD_H5_TEST (shuffle_copy TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# shuffle remove
set (arg ${FILE9} -f dset_shuffle:NONE)
ADD_H5_TEST (shuffle_remove TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# fletcher32 copy
set (arg ${FILE10})
ADD_H5_TEST (fletcher_copy TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# fletcher32 remove
set (arg ${FILE10} -f dset_fletcher32:NONE)
ADD_H5_TEST (fletcher_remove TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# nbit copy
set (arg ${FILE12})
ADD_H5_TEST (nbit_copy TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# nbit remove
set (arg ${FILE12} -f dset_nbit:NONE)
ADD_H5_TEST (nbit_remove TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# nbit add
set (arg ${FILE12} -f dset_int31:NBIT)
ADD_H5_TEST (nbit_add TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# scaleoffset copy
set (arg ${FILE13})
ADD_H5_TEST (scale_copy TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# scaleoffset add
set (arg ${FILE13} -f dset_none:SOFF=31,IN)
ADD_H5_TEST (scale_add TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# scaleoffset remove
set (arg ${FILE13} -f dset_scaleoffset:NONE)
ADD_H5_TEST (scale_remove TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# remove all  filters
set (arg ${FILE11} -f NONE)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (remove_all TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

#filter conversions
set (arg ${FILE8} -f dset_deflate:SZIP=8,NN)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (deflate_convert TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

set (arg ${FILE7} -f dset_szip:GZIP=1)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_SZIP_ENCODER)
  if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
endif ()
ADD_H5_TEST (szip_convert TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

#limit
set (arg ${FILE4} -f GZIP=2 -m 1024)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (deflate_limit TEST_TYPE ${TESTTYPE} RESULT_CODE 0 TEST_FILE ${arg} DUMP_CHECK SIZE_FILTER)

#file
set (arg ${FILE4} -e ${INFO_FILE})
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (deflate_file TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

#crtorder
set (arg tordergr.h5 -L)
set (TESTTYPE "TEST")
ADD_H5_TEST (crtorder TEST_TYPE ${TESTTYPE} RESULT_CODE 0 TEST_FILE ${arg} DUMP_CHECK)

###################################################################################################
# Testing paged aggregation related options:
#   -G pagesize
#   -P 1 or 0
#   -S strategy
#   -T threshold
#
# The testfiles used are generated by test/gen_filespace.c and the file names are prepended with "h5repack_":
#   (1) "fsm_aggr_nopersist.h5"  /* H5F_FSPACE_STRATEGY_FSM_AGGR + not persisting free-space */
#   (2) "fsm_aggr_persist.h5"    /* H5F_FSPACE_STRATEGY_FSM_AGGR + persisting free-space */
#   (3) "paged_nopersist.h5"     /* H5F_FSPACE_STRATEGY_PAGE + not persisting free-space */
#   (4) "paged_persist.h5"       /* H5F_FSPACE_STRATEGY_PAGE + persisting free-space */
#   (5) "aggr.h5"                /* H5F_FSPACE_STRATEGY_AGGR */
#   (6) "none.h5"                /* H5F_FSPACE_STRATEGY_NONE */
#
#####################################################################################################
#
set (arg h5repack_fsm_aggr_nopersist.h5 -S PAGE -P 1)
set (TESTTYPE "TEST")
ADD_H5_TEST (SP_PAGE TEST_TYPE ${TESTTYPE} STAT_RESULT_CODE 0 STAT_ARG SP TEST_FILE ${arg} STAT_CHECK)

set (arg h5repack_fsm_aggr_persist.h5 -S AGGR)
set (TESTTYPE "TEST")
ADD_H5_TEST (S_AGGR TEST_TYPE ${TESTTYPE} STAT_RESULT_CODE 0 STAT_ARG S TEST_FILE ${arg} STAT_CHECK)

set (arg h5repack_none.h5 -S PAGE -T 10 -G 2048)
set (TESTTYPE "TEST")
ADD_H5_TEST (STG_PAGE TEST_TYPE ${TESTTYPE} STAT_RESULT_CODE 0 STAT_ARG STG TEST_FILE ${arg} STAT_CHECK)

set (arg h5repack_paged_nopersist.h5 -G 512 -S AGGR)
set (TESTTYPE "TEST")
ADD_H5_TEST (GS_AGGR TEST_TYPE ${TESTTYPE} STAT_RESULT_CODE 0 STAT_ARG GS TEST_FILE ${arg} STAT_CHECK)

set (arg h5repack_paged_persist.h5 -S NONE -P 1)
set (TESTTYPE "TEST")
ADD_H5_TEST (SP_NONE TEST_TYPE ${TESTTYPE} STAT_RESULT_CODE 0 STAT_ARG SP TEST_FILE ${arg} STAT_CHECK)

set (arg h5repack_aggr.h5 -S FSM_AGGR -P 1 -T 5)
set (TESTTYPE "TEST")
ADD_H5_TEST (SPT_FSM_AGGR TEST_TYPE ${TESTTYPE} STAT_RESULT_CODE 0 STAT_ARG SPT TEST_FILE ${arg} STAT_CHECK)

#########################################################
# layout options (these files have no filters)
#########################################################
ADD_H5_TEST (dset2_chunk_20x10 TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset2 LAYOUT_FILTER CHUNKED --layout=dset2:CHUNK=20x10)
ADD_H5_TEST (chunk_20x10 TEST_TYPE "TEST" RESULT_CODE 1 TEST_FILE ${FILE4} LAYOUT_DSET null LAYOUT_FILTER CHUNKED -l CHUNK=20x10)
ADD_H5_TEST (dset2_conti TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset2 LAYOUT_FILTER CONTIGUOUS -l dset2:CONTI)
ADD_H5_TEST (conti TEST_TYPE "TEST" RESULT_CODE 1 TEST_FILE ${FILE4} LAYOUT_DSET null LAYOUT_FILTER CONTIGUOUS -l CONTI)
ADD_H5_TEST (dset2_compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset2 LAYOUT_FILTER COMPACT -l dset2:COMPA)
ADD_H5_TEST (compa TEST_TYPE "TEST" RESULT_CODE 1 TEST_FILE ${FILE4} LAYOUT_DSET null LAYOUT_FILTER COMPACT -l COMPA)
ADD_H5_TEST (dset2_chunk_20x10-errstk TEST_TYPE "TEST" RESULT_CODE 0 ERR_REF "dimensionality of chunks doesn't match the dataspace" TEST_FILE ${FILE4} --layout=dset2:CHUNK=20x10x5 ERROR_STACK)

################################################################
# layout conversions (file has no filters)
###############################################################
ADD_H5_TEST (dset_compa_conti TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_compact LAYOUT_FILTER CONTIGUOUS -l dset_compact:CONTI)
ADD_H5_TEST (dset_compa_chunk TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_compact LAYOUT_FILTER CHUNKED -l dset_compact:CHUNK=2x5)
ADD_H5_TEST (dset_compa_compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_compact LAYOUT_FILTER COMPACT -l dset_compact:COMPA)
ADD_H5_TEST (dset_conti_compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_contiguous LAYOUT_FILTER COMPACT -l dset_contiguous:COMPA)
ADD_H5_TEST (dset_conti_chunk TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_contiguous LAYOUT_FILTER CHUNKED -l dset_contiguous:CHUNK=3x6)
ADD_H5_TEST (dset_conti_conti TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_contiguous LAYOUT_FILTER CONTIGUOUS -l dset_contiguous:CONTI)
ADD_H5_TEST (chunk_compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_chunk LAYOUT_FILTER COMPACT -l dset_chunk:COMPA)
ADD_H5_TEST (chunk_conti TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_chunk LAYOUT_FILTER CONTIGUOUS -l dset_chunk:CONTI)
ADD_H5_TEST (chunk_18x13 TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE4} LAYOUT_DSET dset_chunk LAYOUT_FILTER CHUNKED -l dset_chunk:CHUNK=18x13)

# test convert small size dataset ( < 1k) to compact layout without -m
ADD_H5_TEST (contig_small_compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE18} LAYOUT_DSET contig_small LAYOUT_FILTER COMPACT -l contig_small:COMPA)
ADD_H5_TEST (contig_small_fixed_compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE18} LAYOUT_DSET chunked_small_fixed LAYOUT_FILTER COMPACT -l chunked_small_fixed:COMPA)

#---------------------------------------------------------------------------
# Test file contains chunked datasets (need multiple dsets) with
# unlimited max dims.   (HDFFV-7933)
# Use first dset to test.
#---------------------------------------------------------------------------
# chunk to chunk - specify chunk dim bigger than any current dim
ADD_H5_TEST (chunk2chunk TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit1 LAYOUT_FILTER CHUNK -l chunk_unlimit1:CHUNK=100x300)

# chunk to contiguous
ADD_H5_TEST (chunk2conti TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit1 LAYOUT_FILTER CONTI -l chunk_unlimit1:CONTI)

# chunk to compact - convert big dataset (should be > 64k) for this purpose,
# should remain as original layout (chunk)
ADD_H5_TEST (chunk2compa TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit1 LAYOUT_FILTER CHUNK -l chunk_unlimit1:COMPA)

#--------------------------------------------------------------------------
# Test -f for some specific cases. Chunked dataset with unlimited max dims.
# (HDFFV-8012)
#--------------------------------------------------------------------------
# - should not fail
# - should not change max dims from unlimit

# chunk dim is bigger than dataset dim. ( dset size < 64k )
ADD_H5_TEST (error1 TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit1 LAYOUT_FILTER H5S_UNLIMITED -f chunk_unlimit1:NONE)

# chunk dim is bigger than dataset dim. ( dset size > 64k )
ADD_H5_TEST (error2 TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit2 LAYOUT_FILTER H5S_UNLIMITED -f chunk_unlimit2:NONE)

# chunk dims are smaller than dataset dims. ( dset size < 64k )
ADD_H5_TEST (error3 TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit3 LAYOUT_FILTER H5S_UNLIMITED -f chunk_unlimit3:NONE)

# file input - should not fail
ADD_H5_TEST (error4 TEST_TYPE "TEST" TEST_FILE ${FILE19} -f NONE ERROR_STACK)

#--------------------------------------------------------------------------
# Test base: Convert CHUNK to CONTI for a chunked dataset with small dataset
# (dset size < 64K) and with unlimited max dims on a condition as follow.
# (HDFFV-8214)
#--------------------------------------------------------------------------
# chunk dim is bigger than dataset dim. should succeed.
ADD_H5_TEST (ckdim_biger TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit2 LAYOUT_FILTER CONTI -l chunk_unlimit2:CONTI)
# chunk dim is smaller than dataset dim. should succeed.
ADD_H5_TEST (ckdim_smaller TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE ${FILE19} LAYOUT_DSET chunk_unlimit3 LAYOUT_FILTER CONTI -l chunk_unlimit3:CONTI)



# Native option
# Do not use FILE1, as the named dtype will be converted to native, and h5diff will
# report a difference.
ADD_H5_TEST (native_fill TEST_TYPE "TEST" TEST_FILE ${FILE0} -n ERROR_STACK)
ADD_H5_TEST (native_attr TEST_TYPE "TEST" TEST_FILE ${FILE2} -n ERROR_STACK)

# latest file format with long switches. use FILE4=h5repack_layout.h5 (no filters)
set (arg --layout CHUNK=20x10 --filter GZIP=1 --minimum=10 --native --latest --compact=8 --indexed=6 --ssize=8[:dtype])
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (layout_long_switches TEST_TYPE ${TESTTYPE} RESULT_CODE 1 TEST_FILE ${FILE4} LAYOUT_DSET null LAYOUT_FILTER CHUNKED ${arg})

# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
set (arg -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (layout_short_switches TEST_TYPE ${TESTTYPE} RESULT_CODE 1 TEST_FILE ${FILE4} LAYOUT_DSET null LAYOUT_FILTER CHUNKED ${arg})

# several global filters
set (arg ${FILE4} --filter GZIP=1 --filter SHUF)
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (global_filters TEST_TYPE ${TESTTYPE} TEST_FILE ${arg} ERROR_STACK)

# syntax of -i infile -o outfile
# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
set (arg ${FILE4} -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
set (TESTTYPE "LEGACY")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (old_style_layout_short_switches TEST_TYPE ${TESTTYPE} TEST_FILE ${arg})

# add a userblock to file
set (arg ${FILE1} -u ${PROJECT_BINARY_DIR}/testfiles/ublock.bin -b 2048)
ADD_H5_TEST (add_userblock TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# add a userblock reserve to file
ADD_H5_VERIFY_USERBLOCK (reserve_userblock 2048 ${FILE1} -b 2048)

# add alignment
set (arg ${FILE1} -t 1 -a 1)
ADD_H5_TEST (add_alignment TEST_TYPE "TEST" TEST_FILE ${arg} ERROR_STACK)

# Check repacking file with old version of layout message (should get upgraded
# to new version and be readable, etc.)
ADD_H5_TEST (upgrade_layout TEST_TYPE "TEST" TEST_FILE ${FILE14} ERROR_STACK NATIVE_ONLY)

# test for datum size > H5TOOLS_MALLOCSIZE
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_TEST (gt_mallocsize TEST_TYPE ${TESTTYPE} TEST_FILE ${FILE1} -f GZIP=1 ERROR_STACK)

# Check repacking file with committed datatypes in odd configurations
ADD_H5_TEST (committed_dt TEST_TYPE "TEST" TEST_FILE ${FILE15} ERROR_STACK)

# tests family driver (file is located in common testfiles folder, uses TOOLTEST1
ADD_H5_TEST (family TEST_TYPE "TEST" TEST_FILE ${FILE16} ERROR_STACK NATIVE_ONLY)

# test various references (bug 1814 and 1726)
ADD_H5_TEST (bug1814 TEST_TYPE "TEST" TEST_FILE ${FILE_REF} ERROR_STACK)

# test attribute with various references (bug1797 / HDFFV-5932)
# the references in attribute of compound or vlen datatype
ADD_H5_TEST (HDFFV-5932 TEST_TYPE "TEST" TEST_FILE ${FILE_ATTR_REF} ERROR_STACK)

# Add test for memory leak in attribute. This test is verified by CTEST.
# 1. leak from vlen string
# 2. leak from compound type without reference member
# (HDFFV-7840, )
# Note: this test is experimental for sharing test file among tools
ADD_H5_TEST (HDFFV-7840 TEST_TYPE "TEST" TEST_FILE h5diff_attr1.h5 ERROR_STACK)

# test CVE-2018-17432 fix
set (arg --low=1 --high=2 -f GZIP=8 -l dset1:CHUNK=5x6)
add_test (
    NAME H5REPACK-HDFFV-10590
    COMMAND $<TARGET_FILE:h5repack> ${arg} ${PROJECT_BINARY_DIR}/testfiles/h5repack_CVE-2018-17432.h5 ${PROJECT_BINARY_DIR}/testfiles/out-HDFFV-10590.h5repack_CVE-2018-17432.h5
)
set_tests_properties (H5REPACK-HDFFV-10590 PROPERTIES
    WILL_FAIL "true"
    ENVIRONMENT "${CROSSCOMPILING_PATH}"
)

# test CVE-2018-14460 fix
add_test (
    NAME H5REPACK-HDFFV-11223
    COMMAND $<TARGET_FILE:h5repack> ${PROJECT_BINARY_DIR}/testfiles/h5repack_CVE-2018-14460.h5 ${PROJECT_BINARY_DIR}/testfiles/out-HDFFV-11223.h5repack_CVE-2018-14460.h5
)
set_tests_properties (H5REPACK-HDFFV-11223 PROPERTIES
    WILL_FAIL "true"
    ENVIRONMENT "${CROSSCOMPILING_PATH}"
)

# tests for metadata block size option ('-M')
ADD_H5_TEST_META (meta_short h5repack_layout.h5 -M 8192)
ADD_H5_TEST_META (meta_long h5repack_layout.h5 --metadata_block_size=8192)

# VDS tests

################################################################
# layout conversions
###############################################################
set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_VERIFY_VDS (vds_dset_chunk20x10x5 ${TESTTYPE} 0 ${FILEV1} vds_dset CHUNKED -l vds_dset:CHUNK=20x10x5)

set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_VERIFY_VDS (vds_chunk2x5x8 ${TESTTYPE} 0 ${FILEV3_1} vds_dset CHUNKED -l vds_dset:CHUNK=2x5x8)

set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_VERIFY_VDS (vds_chunk3x6x9 ${TESTTYPE} 0 ${FILEV2} vds_dset CHUNKED -l vds_dset:CHUNK=3x6x9)

set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_VERIFY_VDS (vds_compa ${TESTTYPE} 0 ${FILEV4} vds_dset COMPACT -l vds_dset:COMPA)

set (TESTTYPE "TEST")
if (NOT USE_FILTER_DEFLATE)
  set (TESTTYPE "SKIP")
endif ()
ADD_H5_VERIFY_VDS (vds_conti ${TESTTYPE} 0 ${FILEV4} vds_dset CONTIGUOUS -l vds_dset:CONTI)

################################################################
# reference new api conversions
###############################################################
ADD_H5_TEST (attrregion TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tattrreg.h5 DUMP_CHECK)
ADD_H5_TEST (dataregion TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tdatareg.h5 DUMP_CHECK)

##############################################################################
###    V E R S I O N  B O U N D S  T E S T S
##############################################################################
# -j 0 -k 2, superblock will be 0
ADD_H5_VERIFY_SUPERBLOCK (SB_IS_0 h5repack_layout.h5 0 2 0)
# -j 1 -k 2, superblock will be 2
ADD_H5_VERIFY_SUPERBLOCK (SB_IS_2 h5repack_layout.h5 1 2 2)
# -j 2 -k 2, superblock will be 3
ADD_H5_VERIFY_SUPERBLOCK (SB_IS_3 h5repack_layout.h5 2 2 3)
# -j 0 -k 1, file cannot be opened
ADD_H5_VERIFY_INVALIDBOUNDS (latest_latest_invalid bounds_latest_latest.h5 0 1)

##############################################################################
###    E X T E R N A L  S T O R A G E  T E S T S
##############################################################################
ADD_H5_EXTERNAL_TEST (ext_f32le "TEST" f32le -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_int32le_1d "TEST" int32le_1d -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_int32le_2d "TEST" int32le_2d -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_int32le_3d "TEST" int32le_3d -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_uint8be "TEST" uint8be -l CONTI)

##############################################################################
###    E X T E R N A L  L I N K  T E S T S
##############################################################################
### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (h5copy_extlinks_src-base TEST_TYPE "SKIP" RESULT_CODE 0 TEST_FILE h5copy_extlinks_src.h5 ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (tsoftlinks-base TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tsoftlinks.h5 ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlink-base TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlink.h5 ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlinkfar-base TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlinkfar.h5 ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlinksrc-base TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlinksrc.h5 ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlinktar-base TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlinktar.h5 ERROR_STACK DUMP_CHECK)

ADD_H5_TEST (h5copy_extlinks_src-merge TEST_TYPE "TEST" DIFF_RESULT_CODE 0 TEST_FILE h5copy_extlinks_src.h5 --merge FULL_DIFF ERROR_STACK)
ADD_H5_TEST (tsoftlinks-merge TEST_TYPE "TEST" DIFF_RESULT_CODE 1 TEST_FILE tsoftlinks.h5 --merge FULL_DIFF ERROR_STACK)
ADD_H5_TEST (textlink-merge TEST_TYPE "TEST" DIFF_RESULT_CODE 0 TEST_FILE textlink.h5 --merge FULL_DIFF ERROR_STACK)

### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (textlinkfar-merge TEST_TYPE "SKIP" DIFF_RESULT_CODE 1 TEST_FILE textlinkfar.h5 --merge FULL_DIFF ERROR_STACK)
### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (textlinksrc-merge TEST_TYPE "SKIP" DIFF_RESULT_CODE 1 TEST_FILE textlinksrc.h5 --merge FULL_DIFF ERROR_STACK)
### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (textlinktar-merge TEST_TYPE "SKIP" DIFF_RESULT_CODE 1 TEST_FILE textlinktar.h5 --merge FULL_DIFF ERROR_STACK)

ADD_H5_TEST (h5copy_extlinks_src-prune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE h5copy_extlinks_src.h5 --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (tsoftlinks-prune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tsoftlinks.h5 --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlink-prune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlink.h5 --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlinkfar-prune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlinkfar.h5 --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlinksrc-prune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlinksrc.h5 --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlinktar-prune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlinktar.h5 --prune ERROR_STACK DUMP_CHECK)

ADD_H5_TEST (h5copy_extlinks_src-mergeprune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE h5copy_extlinks_src.h5 --merge --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (tsoftlinks-mergeprune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tsoftlinks.h5 --merge --prune ERROR_STACK DUMP_CHECK)
ADD_H5_TEST (textlink-mergeprune TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE textlink.h5 --merge --prune ERROR_STACK DUMP_CHECK)
### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (textlinkfar-mergeprune TEST_TYPE "SKIP" RESULT_CODE 0 TEST_FILE textlinkfar.h5 --merge --prune ERROR_STACK DUMP_CHECK)
### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (textlinksrc-mergeprune TEST_TYPE "SKIP" RESULT_CODE 0 TEST_FILE textlinksrc.h5 --merge --prune ERROR_STACK DUMP_CHECK)
### HDFFV-11128 needs fixed to enable the following test
ADD_H5_TEST (textlinktar-mergeprune TEST_TYPE "SKIP" RESULT_CODE 0 TEST_FILE textlinktar.h5 --merge --prune ERROR_STACK DUMP_CHECK)

ADD_H5_TEST (tst_onion_dset_1d TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tst_onion_dset_1d.h5 --src-vfd-name onion --src-vfd-info 1 DUMP_CHECK DUMP_NO_OPT)
ADD_H5_TEST (tst_onion_dset_ext TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tst_onion_dset_ext.h5 --src-vfd-name onion --src-vfd-info 1 DUMP_CHECK DUMP_NO_OPT)
ADD_H5_TEST (tst_onion_objs TEST_TYPE "TEST" RESULT_CODE 0 TEST_FILE tst_onion_objs.h5 --src-vfd-name onion --src-vfd-info 1 DUMP_CHECK DUMP_NO_OPT)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (plugin_version_test 0 h5repack_layout.h5 -v -f UD=260,0,0)
  ADD_H5_UD_TEST (plugin_test 0 h5repack_layout.h5 -v -f UD=257,0,1,9)
  ADD_H5_UD_TEST (plugin_none 0 h5repack_layout.UD.h5 -v -f NONE)
  # check for no parameters
  ADD_H5_UD_TEST (plugin_zero 0 h5repack_layout.h5 -v -f UD=250,0,0)
  # check for less parameters
  ADD_H5_UD_TEST (plugin_test_less 1 h5repack_layout.h5 --enable-error-stack -v -f UD=257,0,1)
  # check for extra parameters
  # could create different macro to grep: h5repack error: incorrect number of compression parameters
  ADD_H5_UD_TEST (plugin_test_ex 1 h5repack_layout.h5 --enable-error-stack -v -f UD=257,0,1,9,9,9)
  # check for extra parameters, which are ignored when nelms is 0
  ADD_H5_UD_TEST (plugin_zero_extra 0 h5repack_layout.h5 --enable-error-stack -v -f UD=250,0,0,1,2)
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
