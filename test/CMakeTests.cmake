
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

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
    file_image_core_test.h5
    fill_old.h5
    fixed_idx.h5
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

# Remove any output file left over from previous test run
ADD_TEST (
    NAME h5test-clear-testhdf5-objects
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
)

IF (HDF5_ENABLE_USING_MEMCHECKER)
  ADD_TEST (NAME testhdf5-base COMMAND $<TARGET_FILE:testhdf5> -x heap -x file -x select)
  SET_TESTS_PROPERTIES(testhdf5-base PROPERTIES DEPENDS h5test-clear-testhdf5-objects)
  SET_TESTS_PROPERTIES(testhdf5-base PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
  ADD_TEST (NAME testhdf5-heap COMMAND $<TARGET_FILE:testhdf5> -o heap)
  SET_TESTS_PROPERTIES(testhdf5-heap PROPERTIES DEPENDS h5test-clear-testhdf5-objects)
  SET_TESTS_PROPERTIES(testhdf5-heap PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
  ADD_TEST (NAME testhdf5-file COMMAND $<TARGET_FILE:testhdf5> -o file)
  SET_TESTS_PROPERTIES(testhdf5-file PROPERTIES DEPENDS h5test-clear-testhdf5-objects)
  SET_TESTS_PROPERTIES(testhdf5-file PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
  ADD_TEST (NAME testhdf5-select COMMAND $<TARGET_FILE:testhdf5> -o select)
  SET_TESTS_PROPERTIES(testhdf5-select PROPERTIES DEPENDS h5test-clear-testhdf5-objects)
  SET_TESTS_PROPERTIES(testhdf5-select PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
ELSE (HDF5_ENABLE_USING_MEMCHECKER)
  ADD_TEST (NAME testhdf5 COMMAND $<TARGET_FILE:testhdf5>)
  SET_TESTS_PROPERTIES(testhdf5 PROPERTIES DEPENDS h5test-clear-testhdf5-objects)
ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

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
        unregister_filter_1.h5
        unregister_filter_2.h5
)

FOREACH (test ${H5_TESTS})
  ADD_TEST (NAME ${test} COMMAND $<TARGET_FILE:${test}>)
  SET_TESTS_PROPERTIES(${test} PROPERTIES DEPENDS h5test-clear-objects)
ENDFOREACH (test ${H5_TESTS})

SET_TESTS_PROPERTIES(flush2 PROPERTIES DEPENDS flush1)

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
ADD_TEST (
    NAME h5test-clear-cache-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_test.h5
)
ADD_TEST (NAME cache COMMAND $<TARGET_FILE:cache>)
SET_TESTS_PROPERTIES(cache PROPERTIES DEPENDS h5test-clear-cache-objects)

#-- Adding test for cache_api
ADD_TEST (
    NAME h5test-clear-cache_api-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_api_test.h5
)
ADD_TEST (NAME cache_api COMMAND $<TARGET_FILE:cache_api>)
SET_TESTS_PROPERTIES(cache_api PROPERTIES DEPENDS h5test-clear-cache_api-objects)

#-- Adding test for ttsafe
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
SET_TESTS_PROPERTIES (error_test PROPERTIES ENVIRONMENT "HDF5_PLUGIN_PRELOAD=::")

#-- Adding test for links_env
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
ADD_TEST (NAME testlibinfo COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIB_TARGET}>" -P "${GREP_RUNNER}")

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
IF (BUILD_SHARED_LIBS)

  IF (WIN32 AND NOT CYGWIN)
    SET(CMAKE_SEP "\;")
  ELSE (WIN32 AND NOT CYGWIN)
    SET(CMAKE_SEP ":")
  ENDIF(WIN32 AND NOT CYGWIN)

  ADD_TEST (NAME H5PLUGIN-plugin COMMAND $<TARGET_FILE:plugin>)
  SET_TESTS_PROPERTIES (H5PLUGIN-plugin PROPERTIES ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir1${CMAKE_SEP}${CMAKE_BINARY_DIR}/testdir2")
ELSE (BUILD_SHARED_LIBS)
  MESSAGE (STATUS " **** Plugins libraries must be built as shared libraries **** ")
  ADD_TEST (
      NAME H5PLUGIN-SKIPPED
      COMMAND ${CMAKE_COMMAND} -E echo "SKIP H5PLUGIN TESTING"
  )
ENDIF (BUILD_SHARED_LIBS)

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
      pool
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
      vfd
      ntypes
      dangle
      dtransform
      reserved
      cross_read
      freespace
      mf
      btree2
      #fheap
      error_test
      err_compat
      tcheck_version
      testmeta
      links_env
      unregister
  )
  IF (NOT CYGWIN)
    SET (H5_VFD_TESTS "${H5_VFD_TESTS} big")
  ENDIF (NOT CYGWIN)
  
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
    TARGET_C_PROPERTIES (${genfile} " " " ")
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
      gen_sizes_lheap
      gen_file_image
  )

  FOREACH (gen ${H5_GENERATORS})
    ADD_H5_GENERATOR (${gen})
  ENDFOREACH (gen ${H5_GENERATORS})

ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
