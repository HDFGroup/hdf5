cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TEST)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (TEST_LIB_SRCS
    ${HDF5_TEST_SOURCE_DIR}/h5test.c
    ${HDF5_TEST_SOURCE_DIR}/testframe.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
)

SET (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (${HDF5_TEST_LIB_TARGET} ${LIB_TYPE} ${TEST_LIB_SRCS} ${TEST_LIB_HEADERS})
IF (MSVC)
  TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "ws2_32.lib")
ENDIF (MSVC)
IF (MINGW)
  TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "wsock32.lib")
ENDIF (MINGW)
TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} ${LIB_TYPE})
SET_TARGET_PROPERTIES (${HDF5_TEST_LIB_TARGET} PROPERTIES FOLDER libraries/test)

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
SET (HDF5_TEST_FILES
  tnullspace.h5
)

FOREACH (h5_tfile ${HDF5_TEST_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${h5_tfile}")
  #MESSAGE (STATUS " Copying ${h5_tfile}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_tfile} ${dest}
  )
ENDFOREACH (h5_tfile ${HDF5_TEST_FILES})

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
SET (HDF5_REFERENCE_FILES
    err_compat_1
    err_compat_2
    error_test_1
    error_test_2
    links_env.out
)

FOREACH (ref_file ${HDF5_REFERENCE_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${ref_file}")
  #MESSAGE (STATUS " Copying ${h5_file}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${XLATE_UTILITY}
      ARGS       ${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file} ${dest} -l3
  )
ENDFOREACH (ref_file ${HDF5_REFERENCE_FILES})

# --------------------------------------------------------------------
# Copy test files from test/testfiles/plist_files dir to test dir
# --------------------------------------------------------------------
SET (HDF5_REFERENCE_PLIST_FILES
    acpl_be
    acpl_le
    dapl_be
    dapl_le
    dcpl_be
    dcpl_le
    dxpl_be
    dxpl_le
    fapl_be
    fapl_le
    fcpl_be
    fcpl_le
    gcpl_be
    gcpl_le
    lapl_be
    lapl_le
    lcpl_be
    lcpl_le
    ocpl_be
    ocpl_le
    ocpypl_be
    ocpypl_le
    strcpl_be
    strcpl_le
)

FOREACH (plistfile ${HDF5_REFERENCE_PLIST_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${plistfile}")
  #MESSAGE (STATUS " Copying ${plistfile} to ${dset}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TEST_SOURCE_DIR}/testfiles/plist_files/${plistfile} ${dest}
  )
ENDFOREACH (plistfile ${HDF5_REFERENCE_PLIST_FILES})

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
SET (HDF5_REFERENCE_TEST_FILES
    be_data.h5
    be_extlink1.h5
    be_extlink2.h5
    corrupt_stab_msg.h5
    deflate.h5
    family_v16_00000.h5
    family_v16_00001.h5
    family_v16_00002.h5
    family_v16_00003.h5
    filespace_1_6.h5
    filespace_1_8.h5
    file_image_core_test.h5
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

FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  SET (dest "${HDF5_TEST_BINARY_DIR}/${h5_file}")
  #MESSAGE (STATUS " Copying ${h5_file} to ${dest}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TEST_SOURCE_DIR}/${h5_file} ${dest}
  )
ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

SET (testhdf5_SRCS
    ${HDF5_TEST_SOURCE_DIR}/testhdf5.c
    ${HDF5_TEST_SOURCE_DIR}/tarray.c
    ${HDF5_TEST_SOURCE_DIR}/tattr.c
    ${HDF5_TEST_SOURCE_DIR}/tchecksum.c
    ${HDF5_TEST_SOURCE_DIR}/tconfig.c
    ${HDF5_TEST_SOURCE_DIR}/tcoords.c
    ${HDF5_TEST_SOURCE_DIR}/tfile.c
    ${HDF5_TEST_SOURCE_DIR}/tgenprop.c
    ${HDF5_TEST_SOURCE_DIR}/th5o.c
    ${HDF5_TEST_SOURCE_DIR}/th5s.c
    ${HDF5_TEST_SOURCE_DIR}/theap.c
    ${HDF5_TEST_SOURCE_DIR}/tid.c
    ${HDF5_TEST_SOURCE_DIR}/titerate.c
    ${HDF5_TEST_SOURCE_DIR}/tmeta.c
    ${HDF5_TEST_SOURCE_DIR}/tmisc.c
    ${HDF5_TEST_SOURCE_DIR}/trefer.c
    ${HDF5_TEST_SOURCE_DIR}/trefstr.c
    ${HDF5_TEST_SOURCE_DIR}/tselect.c
    ${HDF5_TEST_SOURCE_DIR}/tskiplist.c
    ${HDF5_TEST_SOURCE_DIR}/tsohm.c
    ${HDF5_TEST_SOURCE_DIR}/ttime.c
    ${HDF5_TEST_SOURCE_DIR}/ttst.c
    ${HDF5_TEST_SOURCE_DIR}/tunicode.c
    ${HDF5_TEST_SOURCE_DIR}/tvltypes.c
    ${HDF5_TEST_SOURCE_DIR}/tvlstr.c
)

#-- Adding test for testhdf5
ADD_EXECUTABLE (testhdf5 ${testhdf5_SRCS})
TARGET_NAMING (testhdf5 ${LIB_TYPE})
TARGET_LINK_LIBRARIES (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (testhdf5 PROPERTIES FOLDER test)

# Remove any output file left over from previous test run
ADD_TEST (
    NAME h5test-clear-testhdf5-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        coord.h5
        sys_file1
        tattr.h5
        tfile1.h5
        tfile2.h5
        tfile3.h5
        tfile4.h5
        tfile5.h5
        tfile6.h5
        th5o_file
        th5s1.h5
        tselect.h5
        tsohm.h5
        tsohm_dst.h5
        tsohm_src.h5
)
ADD_TEST (NAME testhdf5 COMMAND $<TARGET_FILE:testhdf5>)
SET_TESTS_PROPERTIES(testhdf5 PROPERTIES DEPENDS h5test-clear-testhdf5-objects)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

MACRO (ADD_H5_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (${file} PROPERTIES FOLDER test)

  ADD_TEST (NAME ${file} COMMAND $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5_TEST file)

# Remove any output file left over from previous test run
ADD_TEST (
    NAME h5test-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        dt_arith1.h5
        dt_arith2.h5
        dtransform.h5
        dtypes4.h5
        dtypes5.h5
        efc0.h5
        efc1.h5
        efc2.h5
        efc3.h5
        efc4.h5
        efc5.h5
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
        fheap.h5
        new_multi_file_v16-r.h5
        new_multi_file_v16-s.h5
        objcopy_ext.dat
        testmeta.h5
        tstint1.h5
        tstint2.h5
)

SET (H5_TESTS
    accum
    lheap
    ohdr
    stab
    gheap
    #cache
    #cache_api
    #cache_tagging
    pool
    hyperslab
    istore
    bittests
    dt_arith
    dtypes
    cmpd_dset
    filter_fail
    extend
    external
    efc
    objcopy
    links
    unlink
    big
    mtime
    fillval
    mount
    flush1
    flush2
    app_ref
    enum
    set_extent
    #ttsafe
    getname
    vfd
    ntypes
    dangle
    dtransform
    reserved
    cross_read
    freespace
    mf
    farray
    earray
    btree2
    fheap
    #error_test
    #err_compat
    tcheck_version
    testmeta
    #links_env
    file_image
    enc_dec_plist
    enc_dec_plist_with_endianess
)

FOREACH (test ${H5_TESTS})
  ADD_H5_TEST(${test})
  SET_TESTS_PROPERTIES(${test} PROPERTIES DEPENDS h5test-clear-objects)
ENDFOREACH (test ${H5_TESTS})

SET_TESTS_PROPERTIES(flush2 PROPERTIES DEPENDS flush1)

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
ADD_EXECUTABLE (cache ${HDF5_TEST_SOURCE_DIR}/cache.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache ${LIB_TYPE})
TARGET_LINK_LIBRARIES (cache ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (cache PROPERTIES FOLDER test)
ADD_TEST (
    NAME h5test-clear-cache-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_test.h5
)
ADD_TEST (NAME cache COMMAND $<TARGET_FILE:cache>)
SET_TESTS_PROPERTIES(cache PROPERTIES DEPENDS h5test-clear-cache-objects)

#-- Adding test for cache_api
ADD_EXECUTABLE (cache_api ${HDF5_TEST_SOURCE_DIR}/cache_api.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache_api ${LIB_TYPE})
TARGET_LINK_LIBRARIES (cache_api ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (cache_api PROPERTIES FOLDER test)

ADD_TEST (
    NAME h5test-clear-cache_api-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_api_test.h5
)
ADD_TEST (NAME cache_api COMMAND $<TARGET_FILE:cache_api>)
SET_TESTS_PROPERTIES(cache_api PROPERTIES DEPENDS h5test-clear-cache_api-objects)

#-- Adding test for cache_tagging
ADD_EXECUTABLE (cache_tagging ${HDF5_TEST_SOURCE_DIR}/cache_tagging.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache_tagging ${LIB_TYPE})
TARGET_LINK_LIBRARIES (cache_tagging ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (cache_tagging PROPERTIES FOLDER test)

ADD_TEST (
    NAME h5test-clear-cache_tagging-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        tagging_test.h5
        tagging_ext_test.h5
)
ADD_TEST (NAME cache_tagging COMMAND $<TARGET_FILE:cache_tagging>)
SET_TESTS_PROPERTIES(cache_tagging PROPERTIES DEPENDS h5test-clear-cache_tagging-objects)

#-- Adding test for ttsafe
ADD_EXECUTABLE (ttsafe
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
)
TARGET_NAMING (ttsafe ${LIB_TYPE})
TARGET_LINK_LIBRARIES (ttsafe ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (ttsafe PROPERTIES FOLDER test)

ADD_TEST (
    NAME h5test-clear-ttsafe-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        ttsafe_error.h5
        ttsafe_dcreate.h5
        ttsafe_cancel.h5
        ttsafe_acreate.h5
)
ADD_TEST (NAME ttsafe COMMAND $<TARGET_FILE:ttsafe>)
SET_TESTS_PROPERTIES(ttsafe PROPERTIES DEPENDS h5test-clear-ttsafe-objects)

#-- Adding test for err_compat
IF (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  ADD_EXECUTABLE (err_compat ${HDF5_TEST_SOURCE_DIR}/err_compat.c)
  TARGET_NAMING (err_compat ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (err_compat ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  SET_TARGET_PROPERTIES (err_compat PROPERTIES FOLDER test)

  ADD_TEST (
      NAME h5test-clear-err_compat-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          err_compat.txt
          err_compat.txt.err
  )
  ADD_TEST (NAME err_compat COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:err_compat>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "TEST_OUTPUT=err_compat.txt"
      -D "TEST_REFERENCE=err_compat_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
  )
  SET_TESTS_PROPERTIES(err_compat PROPERTIES DEPENDS h5test-clear-err_compat-objects)
ENDIF (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-- Adding test for error_test
ADD_EXECUTABLE (error_test ${HDF5_TEST_SOURCE_DIR}/error_test.c)
TARGET_NAMING (error_test ${LIB_TYPE})
TARGET_LINK_LIBRARIES (error_test ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (error_test PROPERTIES FOLDER test)

ADD_TEST (
    NAME h5test-clear-error_test-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        error_test.txt
        error_test.txt.err
)
ADD_TEST (NAME error_test COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:error_test>"
    -D "TEST_ARGS:STRING="
    -D "TEST_EXPECT=0"
    -D "TEST_MASK_ERROR=true"
    -D "TEST_OUTPUT=error_test.txt"
    -D "TEST_REFERENCE=error_test_1"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
    -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
)
SET_TESTS_PROPERTIES(error_test PROPERTIES DEPENDS h5test-clear-error_test-objects)

#-- Adding test for links_env
ADD_EXECUTABLE (links_env ${HDF5_TEST_SOURCE_DIR}/links_env.c)
TARGET_NAMING (links_env ${LIB_TYPE})
TARGET_LINK_LIBRARIES (links_env ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (links_env PROPERTIES FOLDER test)

ADD_TEST (
    NAME h5test-clear-links_env-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        links_env.txt
        links_env.txt.err 
        extlinks_env0.h5
        extlinks_env1.h5
        tmp/extlinks_env1.h5
)
ADD_TEST (NAME links_env COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:links_env>"
    -D "TEST_ARGS:STRING="
    -D "TEST_ENV_VAR:STRING=HDF5_EXT_PREFIX"
    -D "TEST_ENV_VALUE:STRING=.:tmp"
    -D "TEST_EXPECT=0"
    -D "TEST_OUTPUT=links_env.txt"
    -D "TEST_REFERENCE=links_env.out"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
    -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
)
SET_TESTS_PROPERTIES(links_env PROPERTIES DEPENDS h5test-clear-links_env-objects)

#-- Adding test for libinfo
SET (GREP_RUNNER ${PROJECT_BINARY_DIR}/GrepRunner.cmake)
FILE (WRITE ${GREP_RUNNER} 
  "FILE (STRINGS \${TEST_PROGRAM} TEST_RESULT REGEX \"SUMMARY OF THE HDF5 CONFIGURATION\")
IF (\${TEST_RESULT} STREQUAL \"0\")
  MESSAGE (FATAL_ERROR \"Failed: The output: \${TEST_RESULT} of \${TEST_PROGRAM} did not contain SUMMARY OF THE HDF5 CONFIGURATION\")
ELSE (\${TEST_RESULT} STREQUAL \"0\")
  MESSAGE (STATUS \"COMMAND Result: \${TEST_RESULT}\")
ENDIF (\${TEST_RESULT} STREQUAL \"0\")
"
)

ADD_TEST (NAME testlibinfo COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIB_TARGET}>" -P "${GREP_RUNNER}")

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

IF (HDF5_TEST_VFD)

  SET (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )

  SET (H5_VFD_TESTS
      testhdf5
      accum
      lheap
      ohdr
      stab
      gheap
      cache
      cache_api
      cache_tagging
      pool
      hyperslab
      istore
      bittests
      dt_arith
      dtypes
      cmpd_dset
      filter_fail
      extend
      external
      efc
      objcopy
      links
      unlink
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
      getname
      vfd
      ntypes
      dangle
      dtransform
      reserved
      cross_read
      freespace
      mf
      farray
      earray
      btree2
      #fheap
      error_test
      err_compat
      tcheck_version
      testmeta
      links_env
)
  
  IF (DIRECT_VFD)
    SET (VFD_LIST ${VFD_LIST} direct)
  ENDIF (DIRECT_VFD)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    FOREACH (test ${H5_VFD_TESTS})
      ADD_TEST (
        NAME VFD-${vfdname}-${test} 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=${test}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
      )
    ENDFOREACH (test ${H5_VFD_TESTS})
    IF (HDF5_TEST_FHEAP_VFD)
      ADD_TEST (
        NAME VFD-${vfdname}-fheap 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:fheap>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=fheap"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
      )
    ENDIF (HDF5_TEST_FHEAP_VFD)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  FOREACH (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  ENDFOREACH (vfd ${VFD_LIST})

ENDIF (HDF5_TEST_VFD)

##############################################################################
##############################################################################
###           T H E   G E N E R A T O R S                                  ###
##############################################################################
##############################################################################

IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  MACRO (ADD_H5_GENERATOR genfile)
    ADD_EXECUTABLE (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    TARGET_NAMING (${genfile} ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (${genfile} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
    SET_TARGET_PROPERTIES (${genfile} PROPERTIES FOLDER generator/test)
  ENDMACRO (ADD_H5_GENERATOR genfile)

  # generator executables
  SET (H5_GENERATORS
      gen_bad_ohdr
      gen_bogus
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

  FOREACH (gen ${H5_GENERATORS})
    ADD_H5_GENERATOR (${gen})
  ENDFOREACH (gen ${H5_GENERATORS})

ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
