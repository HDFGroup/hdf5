
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
  HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/H5TEST/${h5_tfile}" "HDF5_TEST_LIB_files")
endforeach ()

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_FILES
    err_compat_1
    err_compat_2
    error_test_1
    error_test_2
    links_env.out
)

foreach (ref_file ${HDF5_REFERENCE_FILES})
  HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/H5TEST/${ref_file}" "HDF5_TEST_LIB_files")
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
  HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/plist_files/${plistfile}" "${PROJECT_BINARY_DIR}/H5TEST/testfiles/plist_files/${plistfile}" "HDF5_TEST_LIB_files")
  HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/plist_files/def_${plistfile}" "${PROJECT_BINARY_DIR}/H5TEST/testfiles/plist_files/def_${plistfile}" "HDF5_TEST_LIB_files")
endforeach ()

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the test directory into the source directory
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
    deflate.h5
    family_v16_00000.h5
    family_v16_00001.h5
    family_v16_00002.h5
    family_v16_00003.h5
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
    corrupted_name_len.h5
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
    tmtimen.h5
    tmtimeo.h5
    tsizeslheap.h5
)

foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_TEST_BINARY_DIR}/H5TEST/${h5_file}" "HDF5_TEST_LIB_files")
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

add_test (NAME H5TEST-testhdf5-base COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -x heap -x file -x select)
set_tests_properties (H5TEST-testhdf5-base PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-testhdf5-heap COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -o heap)
set_tests_properties (H5TEST-testhdf5-heap PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-testhdf5-file COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -o file)
set_tests_properties (H5TEST-testhdf5-file PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-testhdf5-select COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5> -o select)
set_tests_properties (H5TEST-testhdf5-select PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5
    ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

set (test_CLEANFILES
    accum.h5
    cmpd_dset.h5
    compact_dataset.h5
    dataset.h5
    dset_offset.h5
    max_compact_dataset.h5
    simple.h5
    set_local.h5
    random_chunks.h5
    huge_chunks.h5
    chunk_cache.h5
    big_chunk.h5
    chunk_fast.h5
    chunk_expand.h5
    chunk_fixed.h5
    copy_dcpl_newfile.h5
    partial_chunks.h5
    layout_extend.h5
    zero_chunk.h5
    chunk_single.h5
    swmr_non_latest.h5
    earray_hdr_fd.h5
    farray_hdr_fd.h5
    bt2_hdr_fd.h5
    storage_size.h5
    dls_01_strings.h5
    extend.h5
    istore.h5
    extlinks*.h5
    frspace.h5
    links*.h5
    sys_file1
    tfile*.h5
    th5s*.h5
    lheap.h5
    fheap.h5
    ohdr.h5
    ohdr_min_a.h5
    ohdr_min_b.h5
    min_dset_ohdr_testfile.h5
    stab.h5
    extern_*.h5
    extern_*.raw
    gheap*.h5
    dt_arith1
    dt_arith2
    links.h5
    links*.h5
    extlinks16A00000.h5
    extlinks16A00001.h5
    extlinks16A00002.h5
    extlinks16B-b.h5
    extlinks16B-g.h5
    extlinks16B-l.h5
    extlinks16B-r.h5
    extlinks16B-s.h5
    extlinks19B00000.h5
    extlinks19B00001.h5
    extlinks19B00002.h5
    extlinks19B00003.h5
    extlinks19B00004.h5
    extlinks19B00005.h5
    extlinks19B00006.h5
    extlinks19B00007.h5
    extlinks19B00008.h5
    extlinks19B00009.h5
    extlinks19B00010.h5
    extlinks19B00011.h5
    extlinks19B00012.h5
    extlinks19B00013.h5
    extlinks19B00014.h5
    extlinks19B00015.h5
    extlinks19B00016.h5
    extlinks19B00017.h5
    extlinks19B00018.h5
    extlinks19B00019.h5
    extlinks19B00020.h5
    extlinks19B00021.h5
    extlinks19B00022.h5
    extlinks19B00023.h5
    extlinks19B00024.h5
    extlinks19B00025.h5
    extlinks19B00026.h5
    extlinks19B00027.h5
    extlinks19B00028.h5
    big.data
    big*.h5
    stdio.h5
    sec2.h5
    dtypes0.h5
    dtypes1.h5
    dtypes2.h5
    dtypes3.h5
    dtypes4.h5
    dtypes5.h5
    dtypes6.h5
    dtypes7.h5
    dtypes8.h5
    dtypes9.h5
    dtypes10.h5
    dt_arith1.h5
    dt_arith2.h5
    tattr.h5
    tselect.h5
    mtime.h5
    unlink.h5
    unicode.h5
    coord.h5
    fillval_*.h5
    fillval.raw
    mount_*.h5
    testmeta.h5
    ttime.h5
    trefer1.h5
    trefer2.h5
    trefer3.h5
    tvltypes.h5
    tvlstr.h5
    tvlstr2.h5
    twriteorder.dat
    enum1.h5
    titerate.h5
    ttsafe.h5
    tarray1.h5
    tgenprop.h5
    tmisc*.h5
    set_extent1.h5
    set_extent2.h5
    set_extent3.h5
    set_extent4.h5
    set_extent5.h5
    ext1.bin
    ext2.bin
    getname.h5
    getname1.h5
    getname2.h5
    getname3.h5
    sec2_file.h5
    direct_file.h5
    family_file000*.h5
    new_family_v16_000*.h5
    multi_file-r.h5
    multi_file-s.h5
    core_file
    filter_plugin.h5
    new_move_a.h5
    new_move_b.h5
    ntypes.h5
    dangle.h5
    error_test.h5
    err_compat.h5
    dtransform.h5
    test_filters.h5
    get_file_name.h5
    tstint1.h5
    tstint2.h5
    unlink_chunked.h5
    btree2.h5
    btree2_tmp.h5
    objcopy_src.h5
    objcopy_dst.h5
    objcopy_ext.dat
    trefer1.h5
    trefer2.h5
    app_ref.h5
    farray.h5
    farray_tmp.h5
    earray.h5
    earray_tmp.h5
    efc0.h5
    efc1.h5
    efc2.h5
    efc3.h5
    efc4.h5
    efc5.h5
    log_vfd_out.log
    new_multi_file_v16-r.h5
    new_multi_file_v16-s.h5
    split_get_file_image_test-m.h5
    split_get_file_image_test-r.h5
    file_image_core_test.h5.copy
    unregister_filter_1.h5
    unregister_filter_2.h5
    vds_virt.h5
    vds_dapl.h5
    vds_src_0.h5
    vds_src_1.h5
    swmr_data.h5
    use_use_append_chunk.h5
    use_append_mchunks.h5
    use_disable_mdc_flushes.h5
    tbogus.h5.copy
    flushrefresh.h5
    flushrefresh_VERIFICATION_START
    flushrefresh_VERIFICATION_CHECKPOINT1
    flushrefresh_VERIFICATION_CHECKPOINT2
    flushrefresh_VERIFICATION_DONE
    filenotclosed.h5
    del_many_dense_attrs.h5
    atomic_data
    accum_swmr_big.h5
    ohdr_swmr.h5
    test_swmr*.h5
    cache_logging.h5
    cache_logging.out
    vds_swmr.h5
    vds_swmr_src_*.h5
    tmp_vds_env/vds_src_2.h5
    direct_chunk.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME H5TEST-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${test_CLEANFILES}
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-clear-objects PROPERTIES FIXTURES_SETUP clear_H5TEST)

set (H5TEST_SEPARATE_TESTS
    testhdf5
    cache
    cache_image
    external_env
    flush1
    flush2
    vds_env
)
foreach (h5_test ${H5_TESTS})
  if (NOT h5_test IN_LIST H5TEST_SEPARATE_TESTS)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5TEST-${h5_test} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${h5_test}>)
      set_tests_properties (H5TEST-${h5_test} PROPERTIES
          FIXTURES_REQUIRED clear_objects
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
            -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5TEST-${h5_test} PROPERTIES
          FIXTURES_REQUIRED clear_H5TEST
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    endif ()
  endif ()
endforeach ()

set_tests_properties (H5TEST-fheap PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
set_tests_properties (H5TEST-big PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
set_tests_properties (H5TEST-btree2 PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
set_tests_properties (H5TEST-objcopy PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})

#-- Adding test for cache
if (NOT CYGWIN)
  add_test (
      NAME H5TEST-cache-clear-objects
      COMMAND ${CMAKE_COMMAND} -E remove cache_test.h5
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  set_tests_properties (H5TEST-cache-clear-objects PROPERTIES FIXTURES_SETUP clear_cache)
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5TEST-cache COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:cache>)
  else ()
    add_test (NAME H5TEST-cache COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:cache>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=cache.txt"
        #-D "TEST_REFERENCE=cache.out"
        -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (H5TEST-cache PROPERTIES
      FIXTURES_REQUIRED clear_cache
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  set_tests_properties (H5TEST-cache PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
endif ()

if (TEST_CACHE_IMAGE)
  #-- Adding test for cache_image
  add_test (
      NAME H5TEST-cache_image-clear-objects
      COMMAND ${CMAKE_COMMAND} -E remove cache_image_test.h5
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  set_tests_properties (H5TEST-cache_image-clear-objects PROPERTIES FIXTURES_SETUP clear_cache_image)
  add_test (NAME H5TEST_cache_image COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:cache_image>)
  set_tests_properties (H5TEST-cache_image PROPERTIES
      FIXTURES_REQUIRED clear_cache_image
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
endif ()

#-- Adding test for external_env
add_test (
    NAME H5TEST-external_env-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove
        extern_1r.raw
        extern_2r.raw
        extern_3r.raw
        extern_4r.raw
        extern_1w.raw
        extern_2w.raw
        extern_3w.raw
        extern_4w.raw
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-external_env-clear-objects PROPERTIES FIXTURES_SETUP clear_external_env)
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-external_env COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:external_env>)
else ()
  add_test (NAME H5TEST-external_env COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:external_env>"
        -D "TEST_ARGS:STRING="
        -D "TEST_ENV_VAR:STRING=HDF5_EXTFILE_PREFIX"
        -D "TEST_ENV_VALUE:STRING=\${ORIGIN}"
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=external_env.txt"
        #-D "TEST_REFERENCE=external_env.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (H5TEST-external_env PROPERTIES
    FIXTURES_REQUIRED clear_external_env
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for vds_env
add_test (
    NAME H5TEST-vds_env-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove
        vds_virt_0.h5
        vds_virt_3.h5
        vds_src_2.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-vds_env-clear-objects PROPERTIES FIXTURES_SETUP clear_vds_env)
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-vds_env COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:vds_env>)
else ()
  add_test (NAME H5TEST-vds_env COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:vds_env>"
        -D "TEST_ARGS:STRING="
        -D "TEST_ENV_VAR:STRING=HDF5_VDS_PREFIX"
        -D "TEST_ENV_VALUE:STRING=\${ORIGIN}/tmp_vds_env"
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=vds_env.txt"
        #-D "TEST_REFERENCE=vds_env.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (H5TEST-vds_env PROPERTIES
    FIXTURES_REQUIRED clear_vds_env
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for flush1/2
add_test (NAME H5TEST-flush-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove
        flush.h5
        flush-swmr.h5
        noflush.h5
        noflush-swmr.h5
        flush_extend.h5
        flush_extend-swmr.h5
        noflush_extend.h5
        noflush_extend-swmr.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-flush-clear-objects PROPERTIES FIXTURES_SETUP clear_flush)
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-flush1 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:flush1>)
else ()
  add_test (NAME H5TEST-flush1 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:flush1>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=flush1.txt"
      -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (H5TEST-flush1 PROPERTIES
    FIXTURES_REQUIRED clear_flush
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-flush2 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:flush2>)
else ()
  add_test (NAME H5TEST-flush2 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:flush2>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=flush2.txt"
      -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (H5TEST-flush2 PROPERTIES
    FIXTURES_REQUIRED clear_flush
    DEPENDS H5TEST-flush1
)

#-- Adding test for tcheck_version
add_test (NAME H5TEST-tcheck_version-major COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:tcheck_version> "-tM")
set_tests_properties (H5TEST-tcheck_version-major PROPERTIES
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    WILL_FAIL "true"
)
add_test (NAME H5TEST-tcheck_version-minor COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:tcheck_version> "-tm")
set_tests_properties (H5TEST-tcheck_version-minor PROPERTIES
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    WILL_FAIL "true"
)
add_test (NAME H5TEST-tcheck_version-release COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:tcheck_version> "-tr")
set_tests_properties (H5TEST-tcheck_version-release PROPERTIES
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    WILL_FAIL "true"
)

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
# error_test and err_compat are built at the same time as the other tests, but executed by testerror.sh.
# NOT CONVERTED accum_swmr_reader is used by accum.c.
# NOT CONVERTED atomic_writer and atomic_reader are standalone programs.
# links_env is used by testlinks_env.sh
# filenotclosed and del_many_dense_attrs are used by testabort_fail.sh
# NOT CONVERTED flushrefresh is used by testflushrefresh.sh.
# NOT CONVERTED use_append_chunk, use_append_mchunks and use_disable_mdc_flushes are used by test_usecases.sh
# NOT CONVERTED swmr_* files (besides swmr.c) are used by testswmr.sh.
# NOT CONVERTED vds_swmr_* files are used by testvdsswmr.sh
# NOT CONVERTED 'make check' doesn't run them directly, so they are not included in TEST_PROG.
# NOT CONVERTED Also build testmeta, which is used for timings test.  It builds quickly,
# NOT CONVERTED and this lets automake keep all its test programs in one place.
##############################################################################

#-- Adding test for filenotclosed
add_test (
    NAME H5TEST-filenotclosed-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove filenotclosed.h5
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-filenotclosed-clear-objects PROPERTIES FIXTURES_SETUP clear_filenotclosed)
add_test (NAME H5TEST-filenotclosed COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:filenotclosed>)
set_tests_properties (H5TEST-filenotclosed PROPERTIES
    FIXTURES_REQUIRED clear_filenotclosed
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for del_many_dense_attrs
add_test (
    NAME H5TEST-del_many_dense_attrs-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove del_many_dense_attrs.h5
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-del_many_dense_attrs-clear-objects PROPERTIES FIXTURES_SETUP clear_del_many_dense_attrs)
add_test (NAME H5TEST-del_many_dense_attrs COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:del_many_dense_attrs>)
set_tests_properties (H5TEST-del_many_dense_attrs PROPERTIES
    FIXTURES_REQUIRED clear_del_many_dense_attrs
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for err_compat
if (HDF5_ENABLE_DEPRECATED_SYMBOLS AND NOT MINGW)
  add_test (NAME H5TEST-err_compat COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:err_compat>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "ERROR_APPEND=1"
      -D "TEST_OUTPUT=err_compat.txt"
      -D "TEST_REFERENCE=err_compat_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-err_compat PROPERTIES
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
else ()
  add_test (NAME H5TEST-err_compat COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:err_compat>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "ERROR_APPEND=1"
      -D "TEST_OUTPUT=err_compat.txt"
      -D "TEST_REFERENCE=err_compat_2"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-err_compat PROPERTIES
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
endif ()

#-- Adding test for error_test
if (DEFAULT_API_VERSION MATCHES "v16" OR MINGW)
  add_test (NAME H5TEST-error_test COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:error_test>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "ERROR_APPEND=1"
      -D "TEST_OUTPUT=error_test.txt"
      -D "TEST_REFERENCE=error_test_2"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-error_test PROPERTIES
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5_PLUGIN_PRELOAD=::"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
else ()
  add_test (NAME H5TEST-error_test COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:error_test>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "ERROR_APPEND=1"
      -D "TEST_OUTPUT=error_test.txt"
      -D "TEST_REFERENCE=error_test_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-error_test PROPERTIES
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5_PLUGIN_PRELOAD=::"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
endif ()

#-- Adding test for links_env
add_test (NAME H5TEST-links_env-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove
        extlinks_env0.h5
        extlinks_env1.h5
        tmp_links_env/extlinks_env1.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-links_env-clear-objects PROPERTIES FIXTURES_SETUP clear_links_env)
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-links_env COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:links_env>)
else ()
  add_test (NAME H5TEST-links_env COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:links_env>"
      -D "TEST_ARGS:STRING="
      #-D "TEST_ENV_VAR:STRING=HDF5_EXT_PREFIX"
      #-D "TEST_ENV_VALUE:STRING=.:tmp_links_env"
      -D "TEST_EXPECT=0"
      -D "TEST_OUTPUT=links_env.txt"
      -D "TEST_REFERENCE=links_env.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (H5TEST-links_env PROPERTIES
    FIXTURES_REQUIRED clear_links_env
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5_EXT_PREFIX=.:tmp_links_env"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

if (NOT BUILD_SHARED_LIBS)
  #-- Adding test for libinfo
  add_test (NAME H5TEST-testlibinfo
      COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIB_TARGET}>" -P "${GREP_RUNNER}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
else ()
  #-- Adding test for libinfo
  add_test (NAME H5TEST-testlibinfo
      COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIBSH_TARGET}>" -P "${GREP_RUNNER}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
endif ()

##############################################################################
###    F I L T E R  P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  if (WIN32 OR MINGW)
    set (CMAKE_SEP "\;")
    set (BIN_REL_PATH "../../")
  else ()
    set (CMAKE_SEP ":")
    set (BIN_REL_PATH "../")
  endif ()

  add_test (NAME H5PLUGIN-filter_plugin COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:filter_plugin>)
  set_tests_properties (H5PLUGIN-filter_plugin PROPERTIES
      ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/filter_plugin_dir1${CMAKE_SEP}${CMAKE_BINARY_DIR}/filter_plugin_dir2;srcdir=${HDF5_TEST_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}
  )

##############################################################################
# HDFFV-9655 relative plugin test disabled
#
#  add_test (NAME H5PLUGIN-pluginRelative COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:plugin>)
#  set_tests_properties (H5PLUGIN-pluginRelative PROPERTIES
#      ENVIRONMENT "HDF5_PLUGIN_PATH=@/${BIN_REL_PATH}testdir1${CMAKE_SEP}@/${BIN_REL_PATH}testdir2;srcdir=${HDF5_TEST_BINARY_DIR}"
#      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}
#  )
##############################################################################
endif ()

option (TEST_SHELL_SCRIPTS "Enable shell script tests" OFF)
if (TEST_SHELL_SCRIPTS)
  include (ShellTests.cmake)
endif()

option (ENABLE_EXTENDED_TESTS "Enable extended tests" OFF)
if (ENABLE_EXTENDED_TESTS)
##############################################################################
###    S W M R  T E S T S
##############################################################################
#       testflushrefresh.sh: flushrefresh
#       test_usecases.sh: use_append_chunk, use_append_mchunks, use_disable_mdc_flushes
#       testswmr.sh: swmr*
#       testvdsswmr.sh: vds_swmr*

#  add_test (NAME H5Test-swmr_check_compat_vfd COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:swmr_check_compat_vfd>)

#-- Adding test for flushrefresh
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/flushrefresh_test")
  find_package (Perl)
  if (PERL_FOUND)
    add_test (
        NAME H5TEST-testflushrefresh-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove flushrefresh.h5
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST/flushrefresh_test
    )
    set_tests_properties (H5TEST-testflushrefresh-clear-objects PROPERTIES FIXTURES_SETUP clear_testflushrefresh)
    add_test (NAME H5TEST-testflushrefresh COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:flushrefresh>"
        -D "TEST_ARGS1:STRING=flushrefresh_VERIFICATION_START"
        -D "TEST_ARGS2:STRING=flushrefresh_VERIFICATION_DONE"
        -D "TEST_ERR:STRING=flushrefresh_ERROR"
        -D "TEST_EXPECT=0"
        -D "TEST_OUTPUT=flushrefresh.out"
        -D "TEST_REFERENCE=flushrefresh.txt"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST/flushrefresh_test"
        -D "PERL_SCRIPT=${HDF5_SOURCE_DIR}/bin/runbkgprog"
        -D "PERL_EXECUTABLE=${PERL_EXECUTABLE}"
        -P "${HDF5_TEST_SOURCE_DIR}/flushrefreshTest.cmake"
    )
    set_tests_properties (H5TEST-testflushrefresh PROPERTIES
        FIXTURES_REQUIRED clear_testflushrefresh
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST/flushrefresh_test"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST/flushrefresh_test
    )
  endif ()
else ()
  message (STATUS "Cannot execute TEST flushrefresh - perl not found")
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()

##############################################################################
##############################################################################
###           T H E   G E N E R A T O R S                                  ###
##############################################################################
##############################################################################

if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
  macro (ADD_H5_GENERATOR genfile)
    add_executable (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    target_include_directories (${genfile} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${genfile} STATIC)
    target_link_libraries (${genfile} PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
    set_target_properties (${genfile} PROPERTIES FOLDER generator/test)
  endmacro ()

  # generator executables
  set (H5_GENERATORS
      gen_bad_offset
      gen_bad_ohdr
      gen_bogus
      gen_bounds
      gen_cross
      gen_deflate
      gen_filters
      gen_new_array
      gen_new_fill
      gen_new_group
      gen_new_mtime
      gen_new_super
      gen_noencoder
      gen_nullspace
      gen_udlinks
      space_overflow
      gen_filespace
      gen_specmetaread
      gen_sizes_lheap
      gen_file_image
      gen_plist
  )

  foreach (h5_gen ${H5_GENERATORS})
    ADD_H5_GENERATOR (${h5_gen})
  endforeach ()

endif ()
