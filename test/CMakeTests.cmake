
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# --------------------------------------------------------------------
# Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_TEST_FILES
  tnullspace.h5
)

foreach (h5_tfile ${HDF5_TEST_FILES})
  set (dest "${PROJECT_BINARY_DIR}/${h5_tfile}")
  #message (STATUS " Copying ${h5_tfile}")
  add_custom_command (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_tfile} ${dest}
  )
endforeach (h5_tfile ${HDF5_TEST_FILES})

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
  set (dest "${PROJECT_BINARY_DIR}/${ref_file}")
  #message (STATUS " Copying ${h5_file}")
  add_custom_command (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file} ${dest}
  )
endforeach (ref_file ${HDF5_REFERENCE_FILES})

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_TEST_FILES
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

foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
  set (dest "${HDF5_TEST_BINARY_DIR}/${h5_file}")
  #message (STATUS " Copying ${h5_file} to ${dest}")
  add_custom_command (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TEST_SOURCE_DIR}/${h5_file} ${dest}
  )
endforeach (h5_file ${HDF5_REFERENCE_TEST_FILES})

# Remove any output file left over from previous test run
add_test (
    NAME H5TEST-clear-testhdf5-objects
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

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-testhdf5-base COMMAND $<TARGET_FILE:testhdf5> -x heap -x file -x select)
  set_tests_properties (H5TEST-testhdf5-base PROPERTIES DEPENDS H5TEST-clear-testhdf5-objects)
  set_tests_properties (H5TEST-testhdf5-base PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
  add_test (NAME H5TEST-testhdf5-heap COMMAND $<TARGET_FILE:testhdf5> -o heap)
  set_tests_properties (H5TEST-testhdf5-heap PROPERTIES DEPENDS H5TEST-clear-testhdf5-objects)
  set_tests_properties (H5TEST-testhdf5-heap PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
  add_test (NAME H5TEST-testhdf5-file COMMAND $<TARGET_FILE:testhdf5> -o file)
  set_tests_properties (H5TEST-testhdf5-file PROPERTIES DEPENDS H5TEST-clear-testhdf5-objects)
  set_tests_properties (H5TEST-testhdf5-file PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
  add_test (NAME H5TEST-testhdf5-select COMMAND $<TARGET_FILE:testhdf5> -o select)
  set_tests_properties (H5TEST-testhdf5-select PROPERTIES DEPENDS H5TEST-clear-testhdf5-objects)
  set_tests_properties (H5TEST-testhdf5-select PROPERTIES ENVIRONMENT HDF5_ALARM_SECONDS=3600)
else (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-testhdf5 COMMAND $<TARGET_FILE:testhdf5>)
  set_tests_properties (H5TEST-testhdf5 PROPERTIES DEPENDS H5TEST-clear-testhdf5-objects)
endif (HDF5_ENABLE_USING_MEMCHECKER)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

# Remove any output file left over from previous test run
add_test (
    NAME H5TEST-clear-objects
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

foreach (test ${H5_TESTS})
  add_test (NAME H5TEST-${test} COMMAND $<TARGET_FILE:${test}>)
  set_tests_properties (H5TEST-${test} PROPERTIES DEPENDS H5TEST-clear-objects)
endforeach (test ${H5_TESTS})

set_tests_properties (H5TEST-flush2 PROPERTIES DEPENDS H5TEST-flush1)

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
add_test (
    NAME H5TEST-clear-cache-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_test.h5
)
add_test (NAME H5TEST-cache COMMAND $<TARGET_FILE:cache>)
set_tests_properties (H5TEST-cache PROPERTIES DEPENDS H5TEST-clear-cache-objects)

#-- Adding test for cache_api
add_test (
    NAME H5TEST-clear-cache_api-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        cache_api_test.h5
)
add_test (NAME H5TEST-cache_api COMMAND $<TARGET_FILE:cache_api>)
set_tests_properties (H5TEST-cache_api PROPERTIES DEPENDS H5TEST-clear-cache_api-objects)

#-- Adding test for ttsafe
add_test (
    NAME H5TEST-clear-ttsafe-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        ttsafe_error.h5
        ttsafe_dcreate.h5
        ttsafe_cancel.h5
        ttsafe_acreate.h5
)
add_test (NAME H5TEST-ttsafe COMMAND $<TARGET_FILE:ttsafe>)
set_tests_properties (H5TEST-ttsafe PROPERTIES DEPENDS H5TEST-clear-ttsafe-objects)

#-- Adding test for err_compat
if (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  add_test (
      NAME H5TEST-clear-err_compat-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          err_compat.txt
          err_compat.txt.err
  )
  add_test (NAME H5TEST-err_compat COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:err_compat>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_MASK_ERROR=true"
      -D "TEST_OUTPUT=err_compat.txt"
      -D "TEST_REFERENCE=err_compat_1"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
  set_tests_properties (H5TEST-err_compat PROPERTIES DEPENDS H5TEST-clear-err_compat-objects)
endif (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-- Adding test for error_test
add_test (
    NAME H5TEST-clear-error_test-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        error_test.txt
        error_test.txt.err
)
add_test (NAME H5TEST-error_test COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:error_test>"
    -D "TEST_ARGS:STRING="
    -D "TEST_EXPECT=0"
    -D "TEST_MASK_ERROR=true"
    -D "TEST_OUTPUT=error_test.txt"
    -D "TEST_REFERENCE=error_test_1"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
    -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
)
set_tests_properties (H5TEST-error_test PROPERTIES DEPENDS H5TEST-clear-error_test-objects)
set_tests_properties (H5TEST-error_test PROPERTIES ENVIRONMENT "HDF5_PLUGIN_PRELOAD=::")

#-- Adding test for links_env
add_test (
    NAME H5TEST-clear-links_env-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        links_env.txt
        links_env.txt.err 
        extlinks_env0.h5
        extlinks_env1.h5
        tmp/extlinks_env1.h5
)
add_test (NAME H5TEST-links_env COMMAND "${CMAKE_COMMAND}"
    -D "TEST_PROGRAM=$<TARGET_FILE:links_env>"
    -D "TEST_ARGS:STRING="
    -D "TEST_ENV_VAR:STRING=HDF5_EXT_PREFIX"
    -D "TEST_ENV_VALUE:STRING=.:tmp"
    -D "TEST_EXPECT=0"
    -D "TEST_OUTPUT=links_env.txt"
    -D "TEST_REFERENCE=links_env.out"
    -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
    -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
)
set_tests_properties (H5TEST-links_env PROPERTIES DEPENDS H5TEST-clear-links_env-objects)

#-- Adding test for libinfo
add_test (NAME H5TEST-testlibinfo COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIB_TARGET}>" -P "${GREP_RUNNER}")

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)

  if (WIN32)
    set (CMAKE_SEP "\;")
  else (WIN32)
    set (CMAKE_SEP ":")
  endif (WIN32)

  add_test (NAME H5PLUGIN-plugin COMMAND $<TARGET_FILE:plugin>)
  set_tests_properties (H5PLUGIN-plugin PROPERTIES ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir1${CMAKE_SEP}${CMAKE_BINARY_DIR}/testdir2")
else (BUILD_SHARED_LIBS)
  message (STATUS " **** Plugins libraries must be built as shared libraries **** ")
  add_test (
      NAME H5PLUGIN-SKIPPED
      COMMAND ${CMAKE_COMMAND} -E echo "SKIP H5PLUGIN TESTING"
  )
endif (BUILD_SHARED_LIBS)

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)

  set (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )

  set (H5_VFD_TESTS
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
  if (NOT CYGWIN)
    set (H5_VFD_TESTS ${H5_VFD_TESTS} big)
  endif (NOT CYGWIN)
  
  if (DIRECT_VFD)
    set (VFD_LIST ${VFD_LIST} direct)
  endif (DIRECT_VFD)

  MACRO (CHECK_VFD_TEST vfdtest vfdname resultcode)
    if (${vfdtest} STREQUAL "flush1" OR ${vfdtest} STREQUAL "flush2")
      if (${vfdname} STREQUAL "multi" OR ${vfdname} STREQUAL "split")
        if (NOT BUILD_SHARED_LIBS AND NOT CMAKE_BUILD_TYPE MATCHES Debug)
          add_test (
              NAME VFD-${vfdname}-${vfdtest} 
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
                  -D "TEST_ARGS:STRING="
                  -D "TEST_VFD:STRING=${vfdname}"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_OUTPUT=${vfdtest}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                  -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
          )
        else (NOT BUILD_SHARED_LIBS AND NOT CMAKE_BUILD_TYPE MATCHES Debug)
          add_test (
              NAME VFD-${vfdname}-${vfdtest}
              COMMAND ${CMAKE_COMMAND} -E echo "SKIP VFD-${vfdname}-${vfdtest}"
          )
        endif(NOT BUILD_SHARED_LIBS AND NOT CMAKE_BUILD_TYPE MATCHES Debug)
      else (${vfdname} STREQUAL "multi" OR ${vfdname} STREQUAL "split")
        add_test (
            NAME VFD-${vfdname}-${vfdtest} 
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdtest}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
      endif (${vfdname} STREQUAL "multi" OR ${vfdname} STREQUAL "split")
    else (${vfdtest} STREQUAL "flush1" OR ${vfdtest} STREQUAL "flush2")
      add_test (
          NAME VFD-${vfdname}-${vfdtest} 
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdtest}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
    endif (${vfdtest} STREQUAL "flush1" OR ${vfdtest} STREQUAL "flush2")
  ENDMACRO (CHECK_VFD_TEST vfdtest vfdname resultcode)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    foreach (test ${H5_VFD_TESTS})
      if (WIN32)
        CHECK_VFD_TEST (${test} ${vfdname} ${resultcode})
      else (WIN32)
        add_test (
            NAME VFD-${vfdname}-${test} 
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${test}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
      endif (WIN32)
    endforeach (test ${H5_VFD_TESTS})
    set_tests_properties (VFD-${vfdname}-flush2 PROPERTIES DEPENDS VFD-${vfdname}-flush1)
    set_tests_properties (VFD-${vfdname}-flush1 PROPERTIES TIMEOUT 10)
    set_tests_properties (VFD-${vfdname}-objcopy PROPERTIES TIMEOUT 1000)
    set_tests_properties (VFD-${vfdname}-testhdf5 PROPERTIES TIMEOUT 1200)
    set_tests_properties (VFD-${vfdname}-gheap PROPERTIES TIMEOUT 1200)
    set_tests_properties (VFD-${vfdname}-istore PROPERTIES TIMEOUT 1200)
    if (HDF5_TEST_FHEAP_VFD)
      add_test (
        NAME VFD-${vfdname}-fheap 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:fheap>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=fheap"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
    endif (HDF5_TEST_FHEAP_VFD)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  foreach (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  endforeach (vfd ${VFD_LIST})

endif (HDF5_TEST_VFD)

##############################################################################
##############################################################################
###           T H E   G E N E R A T O R S                                  ###
##############################################################################
##############################################################################

if (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  MACRO (ADD_H5_GENERATOR genfile)
    add_executable (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    TARGET_NAMING (${genfile} ${LIB_TYPE})
    TARGET_C_PROPERTIES (${genfile} " " " ")
    target_link_libraries (${genfile} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
    set_target_properties (${genfile} PROPERTIES FOLDER generator/test)
  ENDMACRO (ADD_H5_GENERATOR genfile)

  # generator executables
  set (H5_GENERATORS
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

  foreach (gen ${H5_GENERATORS})
    ADD_H5_GENERATOR (${gen})
  endforeach (gen ${H5_GENERATORS})

endif (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
