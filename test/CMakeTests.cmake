
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# make test dir
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/testfiles")
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST/testfiles/plist_files")
if (BUILD_SHARED_LIBS)
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST-shared")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST-shared/testfiles")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST-shared/testfiles/plist_files")
endif ()

if (HDF5_TEST_VFD)
  set (VFD_LIST
      sec2
      stdio
      core
      core_paged
      split
      multi
      family
  )
  if (DIRECT_VFD)
    set (VFD_LIST ${VFD_LIST} direct)
  endif ()
  foreach (vfdtest ${VFD_LIST})
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdtest}")
    #if (BUILD_SHARED_LIBS)
    #  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdtest}-shared")
    #endif ()
  endforeach ()
endif ()

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
if (BUILD_SHARED_LIBS)
  add_custom_command (
      TARGET     accum_swmr_reader
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different "$<TARGET_FILE:accum_swmr_reader>" "${PROJECT_BINARY_DIR}/H5TEST-shared/accum_swmr_reader"
  )
endif ()

foreach (h5_tfile ${HDF5_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/H5TEST/${h5_tfile}" "HDF5_TEST_LIB_files")
  if (BUILD_SHARED_LIBS)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/H5TEST-shared/${h5_tfile}" "HDF5_TEST_LIBSH_files")
  endif ()
endforeach ()
if (HDF5_TEST_VFD)
  foreach (vfdtest ${VFD_LIST})
    foreach (h5_tfile ${HDF5_TEST_FILES})
      HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}" "HDF5_TEST_LIB_files")
      if (BUILD_SHARED_LIBS)
        HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/${vfdtest}-shared/${h5_tfile}" "HDF5_TEST_LIBSH_files")
      endif ()
    endforeach ()
  endforeach ()
endif ()

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
  if (BUILD_SHARED_LIBS)
    HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/H5TEST-shared/${ref_file}" "HDF5_TEST_LIBSH_files")
  endif ()
endforeach ()
if (HDF5_TEST_VFD)
  foreach (vfdtest ${VFD_LIST})
    foreach (ref_file ${HDF5_REFERENCE_FILES})
      HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/${vfdtest}/${ref_file}" "HDF5_TEST_LIB_files")
      if (BUILD_SHARED_LIBS)
        HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/${vfdtest}-shared/${ref_file}" "HDF5_TEST_LIBSH_files")
      endif ()
    endforeach ()
  endforeach ()
endif ()

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
  if (BUILD_SHARED_LIBS)
    HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/plist_files/${plistfile}" "${PROJECT_BINARY_DIR}/H5TEST-shared/testfiles/plist_files/${plistfile}" "HDF5_TEST_LIBSH_files")
    HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/plist_files/def_${plistfile}" "${PROJECT_BINARY_DIR}/H5TEST-shared/testfiles/plist_files/def_${plistfile}" "HDF5_TEST_LIBSH_files")
  endif ()
endforeach ()

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_TEST_FILES
    bad_compound.h5
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
    filter_error.h5
    group_old.h5
    le_data.h5
    le_extlink1.h5
    le_extlink2.h5
    mergemsg.h5
    multi_file_v16-r.h5
    multi_file_v16-s.h5
    noencoder.h5
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
  if (BUILD_SHARED_LIBS)
    HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_TEST_BINARY_DIR}/H5TEST-shared/${h5_file}" "HDF5_TEST_LIBSH_files")
  endif ()
endforeach ()
if (HDF5_TEST_VFD)
  foreach (vfdtest ${VFD_LIST})
    foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
      HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_TEST_BINARY_DIR}/${vfdtest}/${h5_file}" "HDF5_TEST_LIB_files")
      if (BUILD_SHARED_LIBS)
        HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_TEST_BINARY_DIR}/${vfdtest}-shared/${h5_file}" "HDF5_TEST_LIBSH_files")
      endif ()
    endforeach ()
  endforeach ()
endif ()
add_custom_target(HDF5_TEST_LIB_files ALL COMMENT "Copying files needed by HDF5_TEST_LIB tests" DEPENDS ${HDF5_TEST_LIB_files_list})
if (BUILD_SHARED_LIBS)
  add_custom_target(HDF5_TEST_LIBSH_files ALL COMMENT "Copying files needed by HDF5_TEST_LIBSH tests" DEPENDS ${HDF5_TEST_LIBSH_files_list})
endif()

# Remove any output file left over from previous test run
add_test (NAME H5TEST-clear-testhdf5-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
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
        th5o_file
        th5s1.h5
        tselect.h5
        tsohm.h5
        tsohm_dst.h5
        tsohm_src.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-testhdf5-base COMMAND $<TARGET_FILE:testhdf5> -x heap -x file -x select)
  set_tests_properties (H5TEST-testhdf5-base PROPERTIES
      DEPENDS H5TEST-clear-testhdf5-objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-testhdf5-heap COMMAND $<TARGET_FILE:testhdf5> -o heap)
  set_tests_properties (H5TEST-testhdf5-heap PROPERTIES
      DEPENDS H5TEST-clear-testhdf5-objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-testhdf5-file COMMAND $<TARGET_FILE:testhdf5> -o file)
  set_tests_properties (H5TEST-testhdf5-file PROPERTIES
      DEPENDS H5TEST-clear-testhdf5-objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-testhdf5-select COMMAND $<TARGET_FILE:testhdf5> -o select)
  set_tests_properties (H5TEST-testhdf5-select PROPERTIES
      DEPENDS H5TEST-clear-testhdf5-objects
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
else ()
  add_test (NAME H5TEST-testhdf5 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=testhdf5.txt"
      #-D "TEST_REFERENCE=testhdf5.out"
      -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-testhdf5 PROPERTIES
      DEPENDS H5TEST-clear-testhdf5-objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  if (BUILD_SHARED_LIBS)
    add_test (NAME H5TEST-shared-clear-testhdf5-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
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
            th5o_file
            th5s1.h5
            tselect.h5
            tsohm.h5
            tsohm_dst.h5
            tsohm_src.h5
        WORKING_DIRECTORY
            ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
    add_test (NAME H5TEST-shared-testhdf5 COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=testhdf5.txt"
        #-D "TEST_REFERENCE=testhdf5.out"
        -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
    set_tests_properties (H5TEST-shared-testhdf5 PROPERTIES
        DEPENDS H5TEST-shared-clear-testhdf5-objects
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
  endif ()
endif ()

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
    tmp
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
    flush.h5
    flush-swmr.h5
    noflush.h5
    noflush-swmr.h5
    flush_extend.h5
    flush_extend-swmr.h5
    noflush_extend.h5
    noflush_extend-swmr.h5
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
    plugin.h5
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
    atomic_data
    accum_swmr_big.h5
    ohdr_swmr.h5
    test_swmr*.h5
    cache_logging.h5
    cache_logging.out
    vds_swmr.h5
    vds_swmr_src_*.h5
)

# Remove any output file left over from previous test run
add_test (NAME H5TEST-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ${test_CLEANFILES}
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)

set (H5TEST_TESTS
    #testhdf5
    #cache
    cache_api
    cache_tagging
    lheap
    ohdr
    stab
    gheap
    evict_on_close
    farray
    earray
    btree2
    fheap
    pool
    accum
    #hyperslab
    istore
    bittests
    dt_arith
    dtypes
    dsets
    cmpd_dset
    filter_fail
    extend
    external
    efc
    objcopy
    links
    unlink
    twriteorder
    big
    mtime
    fillval
    mount
    flush1
    flush2
    app_ref
    enum
    set_extent
    ttsafe
    enc_dec_plist
    enc_dec_plist_cross_platform
    getname
    vfd
    ntypes
    dangle
    dtransform
    reserved
    cross_read
    freespace
    mf
    vds
    file_image
    unregister
    cache_logging
    cork
    swmr
)

foreach (test ${H5TEST_TESTS})
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5TEST-${test} COMMAND $<TARGET_FILE:${test}>)
    set_tests_properties (H5TEST-${test} PROPERTIES
        DEPENDS H5TEST-clear-objects
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
  else ()
    if (${test} STREQUAL "big" AND CYGWIN)
      add_test (NAME H5TEST-${test}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${test}"
      )
    else ()
      add_test (NAME H5TEST-${test} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${test}.txt"
          #-D "TEST_REFERENCE=${test}.out"
          -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5TEST-${test} PROPERTIES
        DEPENDS H5TEST-clear-objects
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
    )
  endif ()
endforeach ()

set_tests_properties (H5TEST-flush2 PROPERTIES DEPENDS H5TEST-flush1)
set_tests_properties (H5TEST-fheap PROPERTIES TIMEOUT 1800)
set_tests_properties (H5TEST-big PROPERTIES TIMEOUT 1800)
set_tests_properties (H5TEST-btree2 PROPERTIES TIMEOUT 1800)
set_tests_properties (H5TEST-objcopy PROPERTIES TIMEOUT 1800)

if (BUILD_SHARED_LIBS)
  # Remove any output file left over from previous test run
  add_test (NAME H5TEST-shared-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          ${test_CLEANFILES}
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )

  foreach (test ${H5TEST_TESTS})
    if (${test} STREQUAL "big" AND CYGWIN)
      add_test (NAME H5TEST-shared-${test}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${test}-shared"
      )
    else ()
      add_test (NAME H5TEST-shared-${test} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${test}-shared>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${test}.txt"
          #-D "TEST_REFERENCE=${test}.out"
          -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5TEST-shared-${test} PROPERTIES
        DEPENDS H5TEST-shared-clear-objects
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
  endforeach ()

  set_tests_properties (H5TEST-shared-flush2 PROPERTIES DEPENDS H5TEST-shared-flush1)
  set_tests_properties (H5TEST-shared-fheap PROPERTIES TIMEOUT 1800)
  set_tests_properties (H5TEST-shared-big PROPERTIES TIMEOUT 1800)
  set_tests_properties (H5TEST-shared-btree2 PROPERTIES TIMEOUT 1800)
  set_tests_properties (H5TEST-shared-objcopy PROPERTIES TIMEOUT 1800)
endif ()

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
if (NOT CYGWIN)
  add_test (NAME H5TEST-clear-cache-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          cache_test.h5
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME H5TEST-cache COMMAND $<TARGET_FILE:cache>)
  else ()
    add_test (NAME H5TEST-cache COMMAND "${CMAKE_COMMAND}"
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
      DEPENDS H5TEST-clear-cache-objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  set_tests_properties (H5TEST-cache PROPERTIES TIMEOUT 1800)
endif ()

#-- Adding test for cache_image
add_test (
    NAME H5TEST-clear-cache_image-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        cache_image_test.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-cache_image COMMAND $<TARGET_FILE:cache_image>)
set_tests_properties (H5TEST-cache_image PROPERTIES
    DEPENDS H5TEST-clear-cache_image-objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for err_compat
if (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  add_test (NAME H5TEST-clear-err_compat-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          err_compat.txt
          err_compat.txt.err
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-err_compat COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:err_compat>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "TEST_OUTPUT=err_compat.txt"
      -D "TEST_REFERENCE=err_compat_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-err_compat PROPERTIES
      DEPENDS H5TEST-clear-err_compat-objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
endif ()

#-- Adding test for error_test
add_test (NAME H5TEST-clear-error_test-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        error_test.txt
        error_test.txt.err
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-error_test COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:error_test>"
    -D "TEST_ARGS:STRING="
    -D "TEST_EXPECT=0"
    -D "TEST_MASK_ERROR=true"
    -D "TEST_OUTPUT=error_test.txt"
    -D "TEST_REFERENCE=error_test_1"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
    -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
)
set_tests_properties (H5TEST-error_test PROPERTIES
    DEPENDS H5TEST-clear-error_test-objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5_PLUGIN_PRELOAD=::"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for links_env
add_test (NAME H5TEST-clear-links_env-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        links_env.txt
        links_env.txt.err
        extlinks_env0.h5
        extlinks_env1.h5
        tmp/extlinks_env1.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-links_env COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:links_env>"
    -D "TEST_ARGS:STRING="
    -D "TEST_ENV_VAR:STRING=HDF5_EXT_PREFIX"
    -D "TEST_ENV_VALUE:STRING=.:tmp"
    -D "TEST_EXPECT=0"
    -D "TEST_OUTPUT=links_env.txt"
    -D "TEST_REFERENCE=links_env.out"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST"
    -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
)
set_tests_properties (H5TEST-links_env PROPERTIES
    DEPENDS H5TEST-clear-links_env-objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for libinfo
add_test (NAME H5TEST-testlibinfo
    COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIB_TARGET}>" -P "${GREP_RUNNER}"
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)

if (BUILD_SHARED_LIBS)
  #-- Adding test for cache
  if (NOT CYGWIN AND NOT WIN32)
    add_test (NAME H5TEST-shared-clear-cache-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            cache_test.h5
        WORKING_DIRECTORY
            ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5TEST-shared-cache COMMAND $<TARGET_FILE:cache-shared>)
    else ()
      add_test (NAME H5TEST-shared-cache COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:cache-shared>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=cache-shared.txt"
          #-D "TEST_REFERENCE=cache-shared.out"
          -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5TEST-shared-cache PROPERTIES
        DEPENDS H5TEST-shared-clear-cache-objects
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared;HDF5TestExpress=${HDF_TEST_EXPRESS}"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
    set_tests_properties (H5TEST-shared-cache PROPERTIES TIMEOUT 1800)
  endif ()

  #-- Adding test for err_compat
  if (HDF5_ENABLE_DEPRECATED_SYMBOLS)
    add_test (NAME H5TEST-shared-clear-err_compat-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            err_compat.txt
            err_compat.txt.err
        WORKING_DIRECTORY
            ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
    add_test (NAME H5TEST-shared-err_compat COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:err_compat-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_MASK_ERROR=true"
        -D "TEST_OUTPUT=err_compat.txt"
        -D "TEST_REFERENCE=err_compat_1"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST-shared"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
    set_tests_properties (H5TEST-shared-err_compat PROPERTIES
        DEPENDS H5TEST-shared-clear-err_compat-objects
        ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
    )
  endif ()

  #-- Adding test for error_test
  add_test (NAME H5TEST-shared-clear-error_test-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          error_test.txt
          error_test.txt.err
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )
  add_test (NAME H5TEST-shared-error_test COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:error_test-shared>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "TEST_OUTPUT=error_test.txt"
      -D "TEST_REFERENCE=error_test_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST-shared"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-shared-error_test PROPERTIES
      DEPENDS H5TEST-shared-clear-error_test-objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared;HDF5_PLUGIN_PRELOAD=::"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )

  #-- Adding test for links_env
  add_test (NAME H5TEST-shared-clear-links_env-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          links_env.txt
          links_env.txt.err
          extlinks_env0.h5
          extlinks_env1.h5
          tmp/extlinks_env1.h5
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )
  add_test (NAME H5TEST-shared-links_env COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:links_env-shared>"
      -D "TEST_ARGS:STRING="
      -D "TEST_ENV_VAR:STRING=HDF5_EXT_PREFIX"
      -D "TEST_ENV_VALUE:STRING=.:tmp"
      -D "TEST_EXPECT=0"
      -D "TEST_OUTPUT=links_env.txt"
      -D "TEST_REFERENCE=links_env.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5TEST-shared"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-shared-links_env PROPERTIES
      DEPENDS H5TEST-shared-clear-links_env-objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )

  #-- Adding test for libinfo
  add_test (NAME H5TEST-shared-testlibinfo
      COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIBSH_TARGET}>" -P "${GREP_RUNNER}"
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )
endif ()

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (WIN32)
  set (CMAKE_SEP "\;")
else ()
  set (CMAKE_SEP ":")
endif ()

add_test (NAME H5PLUGIN-plugin COMMAND $<TARGET_FILE:plugin>)
set_tests_properties (H5PLUGIN-plugin PROPERTIES
    ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir1${CMAKE_SEP}${CMAKE_BINARY_DIR}/testdir2;srcdir=${HDF5_TEST_BINARY_DIR}"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}
)

##############################################################################
###    S W M R  T E S T S
##############################################################################
#       testflushrefresh.sh: flushrefresh
#       test_usecases.sh: use_append_chunk, use_append_mchunks, use_disable_mdc_flushes
#       testswmr.sh: swmr*
#       testvdsswmr.sh: vds_swmr*

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)

  set (H5_VFD_TESTS
      testhdf5
#      cache
      cache_api
      cache_tagging
      lheap
      ohdr
      stab
      gheap
      evict_on_close
      pool
#      accum
      farray
      earray
      btree2
#      fheap
      hyperslab
      istore
      bittests
      dt_arith
      dtypes
      dsets
      cmpd_dset
      filter_fail
      extend
      external
      efc
      objcopy
      links
      unlink
#      big
      mtime
      fillval
      mount
      flush1
      flush2
      app_ref
      enum
      set_extent
      ttsafe
      getname
#      vfd
      ntypes
      dangle
      dtransform
      reserved
      cross_read
      freespace
      mf
#      error_test
#      err_compat
      #tcheck_version
#      testmeta
#      links_env
      unregister
  )
  if (NOT CYGWIN)
    set (H5_VFD_TESTS ${H5_VFD_TESTS} big cache)
  endif ()

  # Windows only macro
  MACRO (CHECK_VFD_TEST vfdtest vfdname resultcode)
    if (${vfdtest} STREQUAL "flush1" OR ${vfdtest} STREQUAL "flush2")
      if (${vfdname} STREQUAL "multi" OR ${vfdname} STREQUAL "split")
        if (NOT BUILD_SHARED_LIBS AND NOT CMAKE_BUILD_TYPE MATCHES Debug)
          add_test (NAME VFD-${vfdname}-${vfdtest}
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
                  -D "TEST_ARGS:STRING="
                  -D "TEST_VFD:STRING=${vfdname}"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_OUTPUT=${vfdname}-${vfdtest}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
                  -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
          )
          set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
              ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
          )
          if (BUILD_SHARED_LIBS)
            add_test (NAME VFD-${vfdname}-${test}-shared
                COMMAND "${CMAKE_COMMAND}"
                    -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}-shared>"
                    -D "TEST_ARGS:STRING="
                    -D "TEST_VFD:STRING=${vfdname}"
                    -D "TEST_EXPECT=${resultcode}"
                    -D "TEST_OUTPUT=${vfdname}-${vfdtest}-shared"
                    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}-shared"
                    -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
            )
            set_tests_properties (VFD-${vfdname}-${vfdtest}-shared PROPERTIES
                ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}-shared"
                WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}-shared
            )
          endif ()
        else ()
          add_test (NAME VFD-${vfdname}-${vfdtest}
              COMMAND ${CMAKE_COMMAND} -E echo "SKIP VFD-${vfdname}-${vfdtest}"
          )
          if (BUILD_SHARED_LIBS)
            add_test (NAME VFD-${vfdname}-${test}-shared
                COMMAND ${CMAKE_COMMAND} -E echo "SKIP VFD-${vfdname}-${vfdtest}-shared"
            )
          endif ()
        endif()
      else ()
        add_test (NAME VFD-${vfdname}-${vfdtest}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdname}-${vfdtest}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
        set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
            ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
        )
        if (BUILD_SHARED_LIBS)
          add_test (NAME VFD-${vfdname}-${test}-shared
              COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}-shared>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdname}-${vfdtest}-shared"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}-shared"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
          )
          set_tests_properties (VFD-${vfdname}-${vfdtest}-shared PROPERTIES
              ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}-shared"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}-shared
          )
        endif ()
      endif ()
    else ()
      add_test (NAME VFD-${vfdname}-${vfdtest}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdname}-${vfdtest}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname};HDF5TestExpress=${HDF_TEST_EXPRESS}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
      )
      if (BUILD_SHARED_LIBS AND NOT ${vfdtest} STREQUAL "cache")
        add_test (NAME VFD-${vfdname}-${vfdtest}-shared
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}-shared>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdname}-${vfdtest}-shared"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}-shared"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
        set_tests_properties (VFD-${vfdname}-${vfdtest}-shared PROPERTIES
            ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}-shared;HDF5TestExpress=${HDF_TEST_EXPRESS}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}-shared
        )
        endif ()
    endif ()
  ENDMACRO ()

  MACRO (ADD_VFD_TEST vfdname resultcode)
    foreach (test ${H5_VFD_TESTS})
      if (WIN32)
        CHECK_VFD_TEST (${test} ${vfdname} ${resultcode})
      else ()
        add_test (NAME VFD-${vfdname}-${test}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdname}-${test}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
        set_tests_properties (VFD-${vfdname}-${test} PROPERTIES
            ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
        )
        if (BUILD_SHARED_LIBS)
          add_test (NAME VFD-${vfdname}-${test}-shared
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:${test}-shared>"
                  -D "TEST_ARGS:STRING="
                  -D "TEST_VFD:STRING=${vfdname}"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_OUTPUT=${vfdname}-${test}-shared"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}-shared"
                  -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
          )
          set_tests_properties (VFD-${vfdname}-${test}-shared PROPERTIES
              ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}-shared"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}-shared
          )
        endif ()
      endif ()
    endforeach ()
    set_tests_properties (VFD-${vfdname}-flush2 PROPERTIES DEPENDS VFD-${vfdname}-flush1)
    set_tests_properties (VFD-${vfdname}-flush1 PROPERTIES TIMEOUT 10)
    set_tests_properties (VFD-${vfdname}-flush2 PROPERTIES TIMEOUT 10)
    set_tests_properties (VFD-${vfdname}-istore PROPERTIES TIMEOUT 1800)
    if (NOT CYGWIN)
      set_tests_properties (VFD-${vfdname}-cache PROPERTIES TIMEOUT 1800)
    endif ()
    if (BUILD_SHARED_LIBS)
      set_tests_properties (VFD-${vfdname}-flush2-shared PROPERTIES DEPENDS VFD-${vfdname}-flush1-shared)
      set_tests_properties (VFD-${vfdname}-flush1-shared PROPERTIES TIMEOUT 10)
      set_tests_properties (VFD-${vfdname}-flush2-shared PROPERTIES TIMEOUT 10)
      set_tests_properties (VFD-${vfdname}-istore-shared PROPERTIES TIMEOUT 1800)
      if (NOT CYGWIN AND NOT WIN32)
        set_tests_properties (VFD-${vfdname}-cache-shared PROPERTIES TIMEOUT 1800)
      endif ()
    endif ()
    if (HDF5_TEST_FHEAP_VFD)
      add_test (NAME VFD-${vfdname}-fheap
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:fheap>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdname}-fheap"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (VFD-${vfdname}-fheap PROPERTIES
          TIMEOUT 1800
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname};HDF5TestExpress=${HDF_TEST_EXPRESS}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
      )
      if (BUILD_SHARED_LIBS)
        add_test (NAME VFD-${vfdname}-fheap-shared
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:fheap-shared>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdname}-fheap-shared"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}-shared"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
        set_tests_properties (VFD-${vfdname}-fheap-shared PROPERTIES
          TIMEOUT 1800
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}-shared;HDF5TestExpress=${HDF_TEST_EXPRESS}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}-shared
      )
      endif ()
    endif ()
  ENDMACRO ()

  # Run test with different Virtual File Driver
  foreach (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  endforeach ()

endif ()

##############################################################################
##############################################################################
###           T H E   G E N E R A T O R S                                  ###
##############################################################################
##############################################################################

if (HDF5_BUILD_GENERATORS)
  MACRO (ADD_H5_GENERATOR genfile)
    add_executable (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    TARGET_NAMING (${genfile} STATIC)
    TARGET_C_PROPERTIES (${genfile} STATIC " " " ")
    target_link_libraries (${genfile} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
    set_target_properties (${genfile} PROPERTIES FOLDER generator/test)
  ENDMACRO ()

  # generator executables
  set (H5_GENERATORS
      gen_bad_ohdr
      gen_bogus
      gen_cross
      gen_deflate
      gen_filters
      gen_idx
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

  foreach (gen ${H5_GENERATORS})
    ADD_H5_GENERATOR (${gen})
  endforeach ()

endif ()
