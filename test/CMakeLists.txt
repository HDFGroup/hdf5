cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TEST)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
set (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
configure_file (${HDF5_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

#-----------------------------------------------------------------------------
# Define Test Library Sources
#-----------------------------------------------------------------------------
set (TEST_LIB_SOURCES
    ${HDF5_TEST_SOURCE_DIR}/h5test.c
    ${HDF5_TEST_SOURCE_DIR}/testframe.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
    ${HDF5_TEST_SOURCE_DIR}/swmr_common.c
)

set (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
    ${HDF5_TEST_SOURCE_DIR}/cache_common.h
    ${HDF5_TEST_SOURCE_DIR}/swmr_common.h
)

add_library (${HDF5_TEST_LIB_TARGET} STATIC ${TEST_LIB_SOURCES} ${TEST_LIB_HEADERS})
TARGET_C_PROPERTIES (${HDF5_TEST_LIB_TARGET} STATIC " " " ")
if (MSVC)
  target_link_libraries (${HDF5_TEST_LIB_TARGET} "ws2_32.lib")
endif ()
if (MINGW)
  target_link_libraries (${HDF5_TEST_LIB_TARGET} "wsock32.lib")
endif ()
target_link_libraries (${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} STATIC)
set_target_properties (${HDF5_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_TEST_LIBSH_TARGET} SHARED ${TEST_LIB_SOURCES} ${TEST_LIB_HEADERS})
  TARGET_C_PROPERTIES (${HDF5_TEST_LIBSH_TARGET} SHARED " " " ")
  if (MSVC)
    target_link_libraries (${HDF5_TEST_LIBSH_TARGET} "ws2_32.lib")
  endif ()
  if (MINGW)
    target_link_libraries (${HDF5_TEST_LIBSH_TARGET} "wsock32.lib")
  endif ()
  target_link_libraries (${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  H5_SET_LIB_OPTIONS (${HDF5_TEST_LIBSH_TARGET} ${HDF5_TEST_LIB_NAME} SHARED ${HDF5_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_TEST_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/test
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
  )
  if (HDF5_ENABLE_THREADSAFE)
    set_property (TARGET ${HDF5_TEST_LIBSH_TARGET}
        APPEND PROPERTY COMPILE_DEFINITIONS
            "H5_HAVE_THREADSAFE"
    )
  endif ()
endif ()

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir1")
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir2")

  #-----------------------------------------------------------------------------
  # Define Plugin Test Sources
  #-----------------------------------------------------------------------------
  set (TEST_PLUGIN_LIBS
      dynlib1
      dynlib3
  )
  set (TEST2_PLUGIN_LIBS
      dynlib2
      dynlib4
  )

  foreach (test_lib ${TEST_PLUGIN_LIBS})
    set (HDF5_TEST_PLUGIN_LIB_CORENAME         "${test_lib}")
    set (HDF5_TEST_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")
    set (HDF5_TEST_PLUGIN_LIB_TARGET           ${HDF5_TEST_PLUGIN_LIB_CORENAME})
    add_definitions (${HDF_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    add_library (${HDF5_TEST_PLUGIN_LIB_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} SHARED " " " ")
    target_link_libraries (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME} SHARED ${HDF5_PACKAGE_SOVERSION})
    set_target_properties (${HDF5_TEST_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_TEST_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
          "$<TARGET_FILE:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/testdir1/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
    )
  endforeach ()

  foreach (test_lib ${TEST2_PLUGIN_LIBS})
    set (HDF5_TEST_PLUGIN_LIB_CORENAME         "${test_lib}")
    set (HDF5_TEST_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")
    set (HDF5_TEST_PLUGIN_LIB_TARGET           ${HDF5_TEST_PLUGIN_LIB_CORENAME})
    add_definitions (${HDF_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    add_library (${HDF5_TEST_PLUGIN_LIB_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} SHARED " " " ")
    target_link_libraries (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME} SHARED ${HDF5_PACKAGE_SOVERSION})
    set_target_properties (${HDF5_TEST_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_TEST_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
          "$<TARGET_FILE:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/testdir2/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
    )
  endforeach ()

set (testhdf5_SOURCES
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
add_executable (testhdf5 ${testhdf5_SOURCES})
TARGET_NAMING (testhdf5 STATIC)
TARGET_C_PROPERTIES (testhdf5 STATIC " " " ")
target_link_libraries (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (testhdf5 PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (testhdf5-shared ${testhdf5_SOURCES})
  TARGET_NAMING (testhdf5-shared SHARED)
  TARGET_C_PROPERTIES (testhdf5-shared SHARED " " " ")
  target_link_libraries (testhdf5-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (testhdf5-shared PROPERTIES FOLDER test)
endif ()

MACRO (ADD_H5_EXE file)
  add_executable (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} STATIC)
  TARGET_C_PROPERTIES (${file} STATIC " " " ")
  target_link_libraries (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (${file} PROPERTIES FOLDER test)
  if (BUILD_SHARED_LIBS)
    add_executable (${file}-shared ${HDF5_TEST_SOURCE_DIR}/${file}.c)
    TARGET_NAMING (${file}-shared SHARED)
    TARGET_C_PROPERTIES (${file}-shared SHARED " " " ")
    target_link_libraries (${file}-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
    set_target_properties (${file}-shared PROPERTIES FOLDER test)
  endif ()
ENDMACRO ()

set (H5_TESTS
    cache
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

foreach (test ${H5_TESTS})
  ADD_H5_EXE(${test})
endforeach ()

set (H5_SWMR_TESTS
    swmr_addrem_writer
    swmr_check_compat_vfd
    swmr_generator
    swmr_reader
    swmr_remove_reader
    swmr_remove_writer
    swmr_sparse_reader
    swmr_sparse_writer
    swmr_start_write
    swmr_writer
)

foreach (test ${H5_SWMR_TESTS})
  ADD_H5_EXE(${test})
endforeach ()

set (H5_VDS_SWMR_TESTS
    vds_swmr_gen
    vds_swmr_reader
    vds_swmr_writer
)

foreach (test ${H5_VDS_SWMR_TESTS})
  ADD_H5_EXE(${test})
endforeach (test ${H5_VDS_SWMR_TESTS})

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache_image
add_executable (cache_image
    ${HDF5_TEST_SOURCE_DIR}/cache_image.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
    ${HDF5_TEST_SOURCE_DIR}/genall5.c
)
TARGET_NAMING (cache_image STATIC)
TARGET_C_PROPERTIES (cache_image STATIC " " " ")
target_link_libraries (cache_image ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (cache_image PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (cache_image-shared
    ${HDF5_TEST_SOURCE_DIR}/cache_image.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
    ${HDF5_TEST_SOURCE_DIR}/genall5.c
  )
  TARGET_NAMING (cache_image-shared SHARED)
  TARGET_C_PROPERTIES (cache_image-shared SHARED " " " ")
  target_link_libraries (cache_image-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (cache_image-shared PROPERTIES FOLDER test)
endif (BUILD_SHARED_LIBS)

#-- Adding test for hyperslab
add_executable (hyperslab ${HDF5_TEST_SOURCE_DIR}/hyperslab.c)
TARGET_NAMING (hyperslab STATIC)
TARGET_C_PROPERTIES (hyperslab STATIC " " " ")
target_link_libraries (hyperslab ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (hyperslab PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (hyperslab-shared ${HDF5_TEST_SOURCE_DIR}/hyperslab.c)
  TARGET_NAMING (hyperslab-shared SHARED)
  TARGET_C_PROPERTIES (hyperslab-shared SHARED " " " ")
  target_link_libraries (hyperslab-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (hyperslab-shared PROPERTIES FOLDER test)
  if (HDF5_ENABLE_THREADSAFE)
    set_property (TARGET hyperslab-shared
        APPEND PROPERTY COMPILE_DEFINITIONS
            "H5_HAVE_THREADSAFE"
    )
  endif ()
endif ()

#-- Adding test for ttsafe
add_executable (ttsafe
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
)
TARGET_NAMING (ttsafe STATIC)
TARGET_C_PROPERTIES (ttsafe STATIC " " " ")
target_link_libraries (ttsafe ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (ttsafe PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (ttsafe-shared
      ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
      ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
      ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
      ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
      ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
  )
  TARGET_NAMING (ttsafe-shared SHARED)
  TARGET_C_PROPERTIES (ttsafe-shared SHARED " " " ")
  target_link_libraries (ttsafe-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (ttsafe-shared PROPERTIES FOLDER test)
  if (HDF5_ENABLE_THREADSAFE)
    set_property (TARGET ttsafe-shared
        APPEND PROPERTY COMPILE_DEFINITIONS
            "H5_HAVE_THREADSAFE"
    )
  endif ()
endif ()

set (H5_CHECK_TESTS
    error_test
    err_compat
    tcheck_version
    testmeta
    atomic_writer
    atomic_reader
    links_env
    flushrefresh
)

foreach (test ${H5_CHECK_TESTS})
  ADD_H5_EXE(${test})
endforeach ()

#-- Adding test for accum_swmr_reader
#   This has to be copied to the test directory for execve() to find it
#   and it can't be renamed (i.e., no <foo>-shared).
add_executable (accum_swmr_reader ${HDF5_TEST_SOURCE_DIR}/accum_swmr_reader.c)
TARGET_NAMING (accum_swmr_reader STATIC)
TARGET_C_PROPERTIES (accum_swmr_reader STATIC " " " ")
target_link_libraries (accum_swmr_reader ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (accum_swmr_reader PROPERTIES FOLDER test)

#-- Set accum dependencies
set_target_properties (accum PROPERTIES DEPENDS accum_swmr_reader)
if (BUILD_SHARED_LIBS)
  set_target_properties (accum-shared PROPERTIES DEPENDS accum_swmr_reader)
endif ()

#-- Adding test for libinfo
set (GREP_RUNNER ${PROJECT_BINARY_DIR}/GrepRunner.cmake)
file (WRITE ${GREP_RUNNER}
  "file (STRINGS \${TEST_PROGRAM} TEST_RESULT REGEX \"SUMMARY OF THE HDF5 CONFIGURATION\")
if (\${TEST_RESULT} STREQUAL \"0\")
  message (FATAL_ERROR \"Failed: The output: \${TEST_RESULT} of \${TEST_PROGRAM} did not contain SUMMARY OF THE HDF5 CONFIGURATION\")
else ()
  message (STATUS \"COMMAND Result: \${TEST_RESULT}\")
endif ()
"
)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  add_executable (plugin ${HDF5_TEST_SOURCE_DIR}/plugin.c)
  TARGET_NAMING (plugin SHARED)
  TARGET_C_PROPERTIES (plugin SHARED " " " ")
  target_link_libraries (plugin ${HDF5_TEST_LIB_TARGET})
  set_target_properties (plugin PROPERTIES FOLDER test)
else ()
  add_executable (plugin ${HDF5_TEST_SOURCE_DIR}/plugin.c)
  TARGET_NAMING (plugin STATIC)
  TARGET_C_PROPERTIES (plugin STATIC " " " ")
  target_link_libraries (plugin ${HDF5_TEST_LIB_TARGET})
  set_target_properties (plugin PROPERTIES FOLDER test)
endif ()

##############################################################################
###    U S E  C A S E S  T E S T S
##############################################################################
set (use_append_chunk_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_append_chunk.c ${HDF5_TEST_SOURCE_DIR}/use_common.c)
add_executable (use_append_chunk ${use_append_chunk_SOURCES})
TARGET_NAMING (use_append_chunk STATIC)
TARGET_C_PROPERTIES (use_append_chunk STATIC " " " ")
target_link_libraries (use_append_chunk ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (use_append_chunk PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (use_append_chunk-shared ${use_append_chunk_SOURCES})
  TARGET_NAMING (use_append_chunk-shared SHARED)
  TARGET_C_PROPERTIES (use_append_chunk-shared SHARED " " " ")
  target_link_libraries (use_append_chunk-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (use_append_chunk-shared PROPERTIES FOLDER test)
endif ()

set (use_append_mchunks_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_append_mchunks.c ${HDF5_TEST_SOURCE_DIR}/use_common.c)
add_executable (use_append_mchunks ${use_append_mchunks_SOURCES})
TARGET_NAMING (use_append_mchunks STATIC)
TARGET_C_PROPERTIES (use_append_mchunks STATIC " " " ")
target_link_libraries (use_append_mchunks ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (use_append_mchunks PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (use_append_mchunks-shared ${use_append_mchunks_SOURCES})
  TARGET_NAMING (use_append_mchunks-shared SHARED)
  TARGET_C_PROPERTIES (use_append_mchunks-shared SHARED " " " ")
  target_link_libraries (use_append_mchunks-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (use_append_mchunks-shared PROPERTIES FOLDER test)
endif ()

set (use_disable_mdc_flushes_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_disable_mdc_flushes.c)
add_executable (use_disable_mdc_flushes ${use_disable_mdc_flushes_SOURCES})
TARGET_NAMING (use_disable_mdc_flushes STATIC)
TARGET_C_PROPERTIES (use_disable_mdc_flushes STATIC " " " ")
target_link_libraries (use_disable_mdc_flushes ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (use_disable_mdc_flushes PROPERTIES FOLDER test)
if (BUILD_SHARED_LIBS)
  add_executable (use_disable_mdc_flushes-shared ${use_disable_mdc_flushes_SOURCES})
  TARGET_NAMING (use_disable_mdc_flushes-shared SHARED)
  TARGET_C_PROPERTIES (use_disable_mdc_flushes-shared SHARED " " " ")
  target_link_libraries (use_disable_mdc_flushes-shared ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (use_disable_mdc_flushes-shared PROPERTIES FOLDER test)
endif ()

include (CMakeTests.cmake)
