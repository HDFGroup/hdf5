
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

# make test dir
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/testfiles")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/testfiles/plist_files")

# --------------------------------------------------------------------
# Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (HDF5_TEST_FILES
  tnullspace.h5
)

add_custom_command (
    TARGET     accum_swmr_reader
    POST_BUILD
    COMMAND    ${CMAKE_COMMAND}
    ARGS       -E copy_if_different "$<TARGET_FILE:accum_swmr_reader>" "${PROJECT_BINARY_DIR}/H5TEST/accum_swmr_reader"
)

foreach (h5_tfile ${HDF5_TEST_FILES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/H5TEST/testfiles/${h5_tfile}" "HDF5_TEST_LIB_files")
endforeach ()

set (HDF5_REFERENCE_FILES
    err_compat_1
    err_compat_2
    error_test_1
    error_test_2
    links_env.out
)

foreach (ref_file ${HDF5_REFERENCE_FILES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/H5TEST/testfiles/${ref_file}" "HDF5_TEST_LIB_files")
endforeach ()

# --------------------------------------------------------------------
# Copy test files from test/testfiles/plist_files dir to test dir
# --------------------------------------------------------------------
set (HDF5_REFERENCE_PLIST_FILES
    acpl_32be
    acpl_32le
    acpl_64be
    acpl_64le
    dapl_32be
    dapl_32le
    dapl_64be
    dapl_64le
    dcpl_32be
    dcpl_32le
    dcpl_64be
    dcpl_64le
    dxpl_32be
    dxpl_32le
    dxpl_64be
    dxpl_64le
    fapl_32be
    fapl_32le
    fapl_64be
    fapl_64le
    fcpl_32be
    fcpl_32le
    fcpl_64be
    fcpl_64le
    gcpl_32be
    gcpl_32le
    gcpl_64be
    gcpl_64le
    lapl_32be
    lapl_32le
    lapl_64be
    lapl_64le
    lcpl_32be
    lcpl_32le
    lcpl_64be
    lcpl_64le
    ocpl_32be
    ocpl_32le
    ocpl_64be
    ocpl_64le
    ocpypl_32be
    ocpypl_32le
    ocpypl_64be
    ocpypl_64le
    strcpl_32be
    strcpl_32le
    strcpl_64be
    strcpl_64le
)

foreach (plistfile ${HDF5_REFERENCE_PLIST_FILES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/plist_files/${plistfile}" "${PROJECT_BINARY_DIR}/H5TEST/testfiles/plist_files/${plistfile}" "HDF5_TEST_LIB_files")
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/plist_files/def_${plistfile}" "${PROJECT_BINARY_DIR}/H5TEST/testfiles/plist_files/def_${plistfile}" "HDF5_TEST_LIB_files")
endforeach ()

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_TEST_FILES
    aggr.h5
    bad_compound.h5
    bad_offset.h5
    be_data.h5
    be_extlink1.h5
    be_extlink2.h5
    btree_idx_1_6.h5
    btree_idx_1_8.h5
    corrupt_stab_msg.h5
    cve_2020_10812.h5
    deflate.h5
    family_v16-000000.h5
    family_v16-000001.h5
    family_v16-000002.h5
    family_v16-000003.h5
    file_image_core_test.h5
    filespace_1_6.h5
    filespace_1_8.h5
    fill_old.h5
    fill18.h5
    filter_error.h5
    fsm_aggr_nopersist.h5
    fsm_aggr_persist.h5
    group_old.h5
    h5fc_ext1_i.h5
    h5fc_ext1_f.h5
    h5fc_ext2_if.h5
    h5fc_ext2_sf.h5
    h5fc_ext3_isf.h5
    h5fc_ext_none.h5
    le_data.h5
    le_extlink1.h5
    le_extlink2.h5
    memleak_H5O_dtype_decode_helper_H5Odtype.h5
    mergemsg.h5
    multi_file_v16-r.h5
    multi_file_v16-s.h5
    noencoder.h5
    none.h5
    paged_nopersist.h5
    paged_persist.h5
    specmetaread.h5
    tarrold.h5
    tbad_msg_count.h5
    tbogus.h5
    test_filters_be.h5
    test_filters_le.h5
    th5s.h5
    tlayouto.h5
    tmisc38a.h5
    tmisc38b.h5
    tmtimen.h5
    tmtimeo.h5
    tsizeslheap.h5
)

foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${HDF5_TEST_BINARY_DIR}/H5TEST/testfiles/${h5_file}" "HDF5_TEST_LIB_files")
endforeach ()

add_custom_target(HDF5_TEST_LIB_files ALL COMMENT "Copying files needed by HDF5_TEST_LIB tests" DEPENDS ${HDF5_TEST_LIB_files_list})

set (testhdf5_CLEANFILES
    coord.h5
    dtypes10.h5
    sys_file1
    tattr.h5
    tfile1.h5
    tfile2.h5
    tfile3.h5
    tfile4.h5
    tfile5.h5
    tfile6.h5
    tfile7.h5
    tfilespace.h5
    th5o_file
    th5s1.h5
    tselect.h5
    tsohm.h5
    tsohm_dst.h5
    tsohm_src.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME H5TEST-testhdf5-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${testhdf5_CLEANFILES}
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-testhdf5-clear-objects PROPERTIES FIXTURES_SETUP clear_testhdf5)
add_test (
    NAME H5TEST-testhdf5-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${testhdf5_CLEANFILES}
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-testhdf5-clean-objects PROPERTIES FIXTURES_CLEANUP clear_testhdf5)

add_test (NAME H5TEST-testhdf5-base COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -x file -x select)
set_tests_properties (H5TEST-testhdf5-base PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
if ("H5TEST-testhdf5-base" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (H5TEST-testhdf5-base PROPERTIES DISABLED true)
endif ()
add_test (NAME H5TEST-testhdf5-file COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -o file)
set_tests_properties (H5TEST-testhdf5-file PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
if ("H5TEST-testhdf5-file" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (H5TEST-testhdf5-file PROPERTIES DISABLED true)
endif ()
add_test (NAME H5TEST-testhdf5-select COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -o select)
set_tests_properties (H5TEST-testhdf5-select PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
if ("H5TEST-testhdf5-select" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (H5TEST-testhdf5-select PROPERTIES DISABLED true)
endif ()

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

set (test_CLEANFILES
    cmpd_dtransform.h5
    direct_chunk.h5
    dt_arith1.h5
    dt_arith2.h5
    dtransform.h5
    dtypes3.h5
    dtypes4.h5
    min_dset_ohdr_testfile.h5
    ohdr_min_a.h5
    sec2_file.h5
    single_latest.h5
    source_file.h5
    splitter_rw_file.h5
    splitter_tmp.h5
    splitter_wo_file.h5
    stdio_file.h5
    swmr0.h5
    tfile_is_accessible_non_hdf5.h5
    tfile_is_accessible.h5
    tfile1.h5.h5
    tfile8.h5
    tfile8.h5.h5
    tmisc15.h5.h5
    tstint1.h5
    tstint2.h5
    tverbounds_dtype.h5
    virtual_file1.h5
    objcopy_ext.dat
    log_vfd_out.log
    splitter.log
    tbogus.h5.copy
    tmp_vds/vds_src_2.h5
)

set (EXTLINKS_CLEANFILES
    extlinks16A-000000.h5
    extlinks16A-000001.h5
    extlinks16B-b.h5
    extlinks16B-g.h5
    extlinks16B-l.h5
    extlinks16B-r.h5
    extlinks16B-s.h5
    extlinks19B-000000.h5
    extlinks19B-000001.h5
    extlinks19B-000002.h5
    extlinks19B-000003.h5
    extlinks19B-000004.h5
    extlinks19B-000005.h5
    extlinks19B-000006.h5
    extlinks19B-000007.h5
    extlinks19B-000008.h5
    extlinks19B-000009.h5
    extlinks19B-000010.h5
    extlinks19B-000011.h5
    extlinks19B-000012.h5
    extlinks19B-000013.h5
    extlinks19B-000014.h5
    extlinks19B-000015.h5
    extlinks19B-000016.h5
    extlinks19B-000017.h5
    extlinks19B-000018.h5
    extlinks19B-000019.h5
    extlinks19B-000020.h5
    extlinks19B-000021.h5
    extlinks19B-000022.h5
    extlinks19B-000023.h5
    extlinks19B-000024.h5
    extlinks19B-000025.h5
    extlinks19B-000026.h5
    extlinks19B-000027.h5
    extlinks19B-000028.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME H5TEST-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${test_CLEANFILES} ${EXTLINKS_CLEANFILES}
    COMMAND_EXPAND_LISTS
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-clear-objects PROPERTIES FIXTURES_SETUP clear_H5TEST)
add_test (
    NAME H5TEST-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${test_CLEANFILES} ${EXTLINKS_CLEANFILES}
    COMMAND_EXPAND_LISTS
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-clean-objects PROPERTIES FIXTURES_CLEANUP clear_H5TEST)

set (H5TEST_SEPARATE_TESTS
    testhdf5
    cache
    cache_image
    external_env
    flush1
    flush2
    vds_env
)
foreach (h5_test ${H5_EXPRESS_TESTS})
  if (NOT h5_test IN_LIST H5TEST_SEPARATE_TESTS)
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5TESTXPR-${h5_test} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${h5_test}>)
      set_tests_properties (H5TESTXPR-${h5_test} PROPERTIES
          FIXTURES_REQUIRED clear_H5TEST
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    else ()
      add_test (NAME H5TESTXPR-${h5_test} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${h5_test}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${h5_test}.txt"
          -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
          #-D "TEST_REFERENCE=${test}.out"
          -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
          -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5TESTXPR-${h5_test} PROPERTIES
          FIXTURES_REQUIRED clear_H5TEST
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    endif ()
    if ("H5TESTXPR-${h5_test}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5TESTXPR-${h5_test} PROPERTIES DISABLED true)
    endif ()
  endif ()
endforeach ()
foreach (h5_test ${H5_TESTS})
  if (NOT h5_test IN_LIST H5TEST_SEPARATE_TESTS)
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5TEST-${h5_test} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${h5_test}>)
      set_tests_properties (H5TEST-${h5_test} PROPERTIES
          FIXTURES_REQUIRED clear_H5TEST
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    else ()
      if ("${h5_test}" STREQUAL "big" AND CYGWIN)
        add_test (NAME H5TEST-${h5_test}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${test}"
        )
      else ()
        add_test (NAME H5TEST-${h5_test} COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${h5_test}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_OUTPUT=${h5_test}.txt"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            #-D "TEST_REFERENCE=${test}.out"
            -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
            -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5TEST-${h5_test} PROPERTIES
          FIXTURES_REQUIRED clear_H5TEST
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    endif ()
    if ("H5TEST-${h5_test}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5TEST-${h5_test} PROPERTIES DISABLED true)
    endif ()
  endif ()
endforeach ()

#set_tests_properties (H5TESTXPR-fheap PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
#set_tests_properties (H5TEST-big PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
#set_tests_properties (H5TESTXPR-btree2 PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
#set_tests_properties (H5TESTXPR-objcopy PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})

#-- Adding test for cache
if (NOT CYGWIN)
endif ()

if (TEST_CACHE_IMAGE)
endif ()


##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################
# H5_CHECK_TESTS
#---------------
#    error_test
#    err_compat
#    testmeta
#    atomic_writer
#    atomic_reader
#    links_env
#    filenotclosed
#    del_many_dense_attrs
#    flushrefresh
##############################################################################
# autotools script tests
# error_test and err_compat are built at the same time as the other tests, but executed by test_error.sh
# NOT CONVERTED accum_swmr_reader is used by accum.c
# NOT CONVERTED atomic_writer and atomic_reader are stand-alone programs
# links_env is used by test_links_env.sh
# filenotclosed and del_many_dense_attrs are used by test_abort_fail.sh
# NOT CONVERTED flushrefresh is used by test_flush_refresh.sh
# NOT CONVERTED use_append_chunk, use_append_mchunks and use_disable_mdc_flushes are used by test_use_cases.sh
# NOT CONVERTED swmr_* files (besides swmr.c) are used by test_swmr.sh
# NOT CONVERTED vds_swmr_* files are used by test_vds_swmr.sh
# 'make check' doesn't run them directly, so they are not included in TEST_PROG.
# Also build testmeta, which is used for timings test.  It builds quickly
# and this lets automake keep all its test programs in one place.
##############################################################################

