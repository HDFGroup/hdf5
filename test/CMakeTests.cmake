
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
if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST-shared")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST-shared/testfiles")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5TEST-shared/testfiles/plist_files")
endif ()

# --------------------------------------------------------------------
# Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (HDF5_TEST_FILES
  tnullspace.h5
)

foreach (h5_tfile ${HDF5_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/H5TEST/${h5_tfile}" "HDF5_TEST_LIB_files")
  if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/H5TEST-shared/${h5_tfile}" "HDF5_TEST_LIBSH_files")
  endif ()
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
  if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
    HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/H5TEST-shared/${ref_file}" "HDF5_TEST_LIBSH_files")
  endif ()
endforeach ()

# --------------------------------------------------------------------
#-- Copy all the HDF5 files from the test directory into the source directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_TEST_FILES
    bad_compound.h5
    bad_offset.h5
    be_data.h5
    be_extlink1.h5
    be_extlink2.h5
    bounds_earliest_latest.h5
    bounds_earliest_v18.h5
    bounds_latest_latest.h5
    bounds_v18_latest.h5
    bounds_v18_v18.h5
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
  HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_TEST_BINARY_DIR}/H5TEST/${h5_file}" "HDF5_TEST_LIB_files")
  if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
    HDFTEST_COPY_FILE("${HDF5_TEST_SOURCE_DIR}/${h5_file}" "${HDF5_TEST_BINARY_DIR}/H5TEST-shared/${h5_file}" "HDF5_TEST_LIBSH_files")
  endif ()
endforeach ()

add_custom_target(HDF5_TEST_LIB_files ALL COMMENT "Copying files needed by HDF5_TEST_LIB tests" DEPENDS ${HDF5_TEST_LIB_files_list})
if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
  add_custom_target(HDF5_TEST_LIBSH_files ALL COMMENT "Copying files needed by HDF5_TEST_LIBSH tests" DEPENDS ${HDF5_TEST_LIBSH_files_list})
endif ()

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
set_tests_properties (H5TEST-clear-testhdf5-objects PROPERTIES FIXTURES_SETUP clear_testhdf5)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-testhdf5-base COMMAND $<TARGET_FILE:testhdf5> -x heap -x file -x select)
  set_tests_properties (H5TEST-testhdf5-base PROPERTIES
      FIXTURES_REQUIRED clear_testhdf5
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-testhdf5-heap COMMAND $<TARGET_FILE:testhdf5> -o heap)
  set_tests_properties (H5TEST-testhdf5-heap PROPERTIES
      FIXTURES_REQUIRED clear_testhdf5
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-testhdf5-file COMMAND $<TARGET_FILE:testhdf5> -o file)
  set_tests_properties (H5TEST-testhdf5-file PROPERTIES
      FIXTURES_REQUIRED clear_testhdf5
      ENVIRONMENT "HDF5_ALARM_SECONDS=3600;srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  add_test (NAME H5TEST-testhdf5-select COMMAND $<TARGET_FILE:testhdf5> -o select)
  set_tests_properties (H5TEST-testhdf5-select PROPERTIES
      FIXTURES_REQUIRED clear_testhdf5
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
      FIXTURES_REQUIRED clear_testhdf5
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
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
    set_tests_properties (H5TEST-shared-clear-testhdf5-objects PROPERTIES FIXTURES_SETUP shared_clear_testhdf5)
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
        FIXTURES_REQUIRED shared_clear_testhdf5
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
    dt_arith1.h5
    dt_arith2.h5
    dtransform.h5
    dtypes3.h5
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
    log_vfd_out.log
    new_multi_file_v16-r.h5
    new_multi_file_v16-s.h5
    objcopy_ext.dat
    testmeta.h5
    tstint1.h5
    tstint2.h5
    unregister_filter_1.h5
    unregister_filter_2.h5
)

# Remove any output file left over from previous test run
add_test (NAME H5TEST-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ${test_CLEANFILES}
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
set_tests_properties (H5TEST-clear-objects PROPERTIES FIXTURES_SETUP clear_objects)

set (H5TEST_SEPARATE_TESTS
    testhdf5
    cache
    cache_image
    flush1
    flush2
)
foreach (test ${H5_TESTS})
  if (NOT ${test} IN_LIST H5TEST_SEPARATE_TESTS)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5TEST-${test} COMMAND $<TARGET_FILE:${test}>)
      set_tests_properties (H5TEST-${test} PROPERTIES
          FIXTURES_REQUIRED clear_objects
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    else ()
      if ("${test}" STREQUAL "big" AND CYGWIN)
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
          FIXTURES_REQUIRED clear_objects
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
      )
    endif ()
  endif ()
endforeach ()

set_tests_properties (H5TEST-fheap PROPERTIES TIMEOUT 1800)
set_tests_properties (H5TEST-testmeta PROPERTIES TIMEOUT 1800)
set_tests_properties (H5TEST-big PROPERTIES TIMEOUT 1800)
set_tests_properties (H5TEST-objcopy PROPERTIES TIMEOUT 2400)

if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
  # Remove any output file left over from previous test run
  add_test (NAME H5TEST-shared-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          ${test_CLEANFILES}
      WORKING_DIRECTORY
          ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
  )
  set_tests_properties (H5TEST-shared-clear-objects PROPERTIES FIXTURES_SETUP shared_clear_objects)

  foreach (test ${H5_TESTS})
    if (NOT ${test} IN_LIST H5TEST_SEPARATE_TESTS)
      if ("${test}" STREQUAL "big" AND CYGWIN)
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
          FIXTURES_REQUIRED shared_clear_objects
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST-shared"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST-shared
      )
    endif ()
  endforeach ()

  set_tests_properties (H5TEST-shared-fheap PROPERTIES TIMEOUT 1800)
  set_tests_properties (H5TEST-shared-testmeta PROPERTIES TIMEOUT 1800)
  set_tests_properties (H5TEST-shared-big PROPERTIES TIMEOUT 1800)
  set_tests_properties (H5TEST-shared-objcopy PROPERTIES TIMEOUT 2400)
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
  set_tests_properties (H5TEST-clear-cache-objects PROPERTIES FIXTURES_SETUP cache_clear_objects)
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
      FIXTURES_REQUIRED cache_clear_objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
  set_tests_properties (H5TEST-cache PROPERTIES TIMEOUT 1800)
endif ()

#-- Adding test for cache_api
add_test (
    NAME H5TEST-clear-cache_api-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        cache_api_test.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-cache_api COMMAND $<TARGET_FILE:cache_api>)
set_tests_properties (H5TEST-cache_api PROPERTIES
    DEPENDS H5TEST-clear-cache_api-objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for ttsafe
add_test (
    NAME H5TEST-clear-ttsafe-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        ttsafe_error.h5
        ttsafe_dcreate.h5
        ttsafe_cancel.h5
        ttsafe_acreate.h5
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)
add_test (NAME H5TEST-ttsafe COMMAND $<TARGET_FILE:ttsafe>)
set_tests_properties (H5TEST-ttsafe PROPERTIES
    DEPENDS H5TEST-clear-ttsafe-objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for flush1/2
add_test (NAME H5TEST-clear-flush-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
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
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-flush1 COMMAND $<TARGET_FILE:flush1>)
else ()
  add_test (NAME H5TEST-flush1 COMMAND "${CMAKE_COMMAND}"
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
    DEPENDS H5TEST-clear-flush-objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5TestExpress=${HDF_TEST_EXPRESS}"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME H5TEST-flush2 COMMAND $<TARGET_FILE:flush2>)
else ()
  add_test (NAME H5TEST-flush2 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:flush2>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=flush2.txt"
      -D "TEST_FOLDER=${HDF5_TEST_BINARY_DIR}/H5TEST"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (H5TEST-flush2 PROPERTIES DEPENDS H5TEST-flush1)

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
  set_tests_properties (H5TEST-clear-err_compat-objects PROPERTIES FIXTURES_SETUP err_compat_clear_objects)
  add_test (NAME H5TEST-err_compat COMMAND "${CMAKE_COMMAND}"
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
      FIXTURES_REQUIRED err_compat_clear_objects
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
set_tests_properties (H5TEST-clear-error_test-objects PROPERTIES FIXTURES_SETUP error_test_clear_objects)
if (HDF5_USE_16_API_DEFAULT)
  add_test (
      NAME H5TEST-error_test
      COMMAND ${CMAKE_COMMAND} -E echo "SKIP $<TARGET_FILE:error_test>"
  )
  set_property(TEST H5TEST-error_test PROPERTY DISABLED)
else ()
  add_test (NAME H5TEST-error_test COMMAND "${CMAKE_COMMAND}"
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
      FIXTURES_REQUIRED error_test_clear_objects
      ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST;HDF5_PLUGIN_PRELOAD=::"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
  )
endif ()

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
set_tests_properties (H5TEST-clear-links_env-objects PROPERTIES FIXTURES_SETUP links_env_clear_objects)
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
    FIXTURES_REQUIRED links_env_clear_objects
    ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/H5TEST"
    WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/H5TEST
)

#-- Adding test for libinfo
add_test (NAME H5TEST-testlibinfo
    COMMAND ${CMAKE_COMMAND} -D "TEST_PROGRAM=$<TARGET_FILE:${HDF5_LIB_TARGET}>" -P "${GREP_RUNNER}"
    WORKING_DIRECTORY
        ${HDF5_TEST_BINARY_DIR}/H5TEST
)

##############################################################################
###    F I L T E R  P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS AND TEST_SHARED_PROGRAMS)
  if (WIN32)
    set (CMAKE_SEP "\;")
    set (BIN_REL_PATH "../../")
  else ()
    set (CMAKE_SEP ":")
    set (BIN_REL_PATH "../")
  endif ()

  add_test (NAME H5PLUGIN-plugin COMMAND $<TARGET_FILE:filter_plugin>)
  set_tests_properties (H5PLUGIN-plugin PROPERTIES
      ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/testdir1${CMAKE_SEP}${CMAKE_BINARY_DIR}/testdir2;srcdir=${HDF5_TEST_BINARY_DIR}"
      WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}
  )
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

if (HDF5_BUILD_GENERATORS)
  macro (ADD_H5_GENERATOR genfile)
    add_executable (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    target_include_directories(${genfile} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${genfile} STATIC)
    target_link_libraries (${genfile} PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
    set_target_properties (${genfile} PROPERTIES FOLDER generator/test)
  endmacro ()

  # generator executables
  set (H5_GENERATORS
      gen_bad_offset
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
      gen_sizes_lheap
      gen_file_image
  )

  foreach (gen ${H5_GENERATORS})
    ADD_H5_GENERATOR (${gen})
  endforeach ()

endif ()
