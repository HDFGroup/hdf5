cmake_minimum_required (VERSION 2.8.11)
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
set (TEST_LIB_SRCS
    ${HDF5_TEST_SOURCE_DIR}/h5test.c
    ${HDF5_TEST_SOURCE_DIR}/testframe.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
)

set (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
)

add_library (${HDF5_TEST_LIB_TARGET} ${LIB_TYPE} ${TEST_LIB_SRCS} ${TEST_LIB_HEADERS})
TARGET_C_PROPERTIES (${HDF5_TEST_LIB_TARGET} " " " ")
if (MSVC)
  target_link_libraries (${HDF5_TEST_LIB_TARGET} "ws2_32.lib")
endif (MSVC)
if (MINGW)
  target_link_libraries (${HDF5_TEST_LIB_TARGET} "wsock32.lib")
endif (MINGW)
target_link_libraries (${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} ${LIB_TYPE})
set_target_properties (${HDF5_TEST_LIB_TARGET} PROPERTIES
    FOLDER libraries/test
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir1")
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir2")

  #-----------------------------------------------------------------------------
  # Define Test Library Sources
  #-----------------------------------------------------------------------------
  set (TEST_PLUGIN_LIBS
      dynlib1
      dynlib3
  )
  set (TEST2_PLUGIN_LIBS
      dynlib2
  )

  foreach (test_lib ${TEST_PLUGIN_LIBS})
    set (HDF5_TEST_PLUGIN_LIB_CORENAME         "${test_lib}")
    set (HDF5_TEST_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")
    set (HDF5_TEST_PLUGIN_LIB_TARGET           ${HDF5_TEST_PLUGIN_LIB_CORENAME})
    add_definitions (${HDF_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    add_library (${HDF5_TEST_PLUGIN_LIB_TARGET} ${LIB_TYPE} ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} " " " ")
    target_link_libraries (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (
        ${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME}
        ${LIB_TYPE}
        HDF5_TEST_PLUGIN_LIB_NAME_RELEASE
        HDF5_TEST_PLUGIN_LIB_NAME_DEBUG
    )
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
  endforeach (test_lib ${TEST_PLUGIN_LIBS})

  foreach (test_lib ${TEST2_PLUGIN_LIBS})
    set (HDF5_TEST_PLUGIN_LIB_CORENAME         "${test_lib}")
    set (HDF5_TEST_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")
    set (HDF5_TEST_PLUGIN_LIB_TARGET           ${HDF5_TEST_PLUGIN_LIB_CORENAME})
    add_definitions (${HDF_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    add_library (${HDF5_TEST_PLUGIN_LIB_TARGET} ${LIB_TYPE} ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} " " " ")
    target_link_libraries (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (
        ${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME}
        ${LIB_TYPE}
        HDF5_TEST_PLUGIN_LIB_NAME_RELEASE
        HDF5_TEST_PLUGIN_LIB_NAME_DEBUG
    )
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
  endforeach (test_lib ${TEST2_PLUGIN_LIBS})
endif (BUILD_SHARED_LIBS)

set (testhdf5_SRCS
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
add_executable (testhdf5 ${testhdf5_SRCS})
TARGET_NAMING (testhdf5 ${LIB_TYPE})
TARGET_C_PROPERTIES (testhdf5 " " " ")
target_link_libraries (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (testhdf5 PROPERTIES FOLDER test)

MACRO (ADD_H5_EXE file)
  add_executable (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_C_PROPERTIES (${file} " " " ")
  target_link_libraries (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (${file} PROPERTIES FOLDER test)
ENDMACRO (ADD_H5_EXE file)

set (H5_TESTS
    accum
    lheap
    ohdr
    stab
    gheap
    #cache
    #cache_api
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
#    big
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
    btree2
    fheap
    #error_test
    #err_compat
    tcheck_version
    testmeta
    #links_env
    file_image
    unregister
)
if (NOT CYGWIN)
  set (H5_TESTS ${H5_TESTS} big)
endif (NOT CYGWIN)

foreach (test ${H5_TESTS})
  ADD_H5_EXE(${test})
endforeach (test ${H5_TESTS})

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
add_executable (cache ${HDF5_TEST_SOURCE_DIR}/cache.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache ${LIB_TYPE})
TARGET_C_PROPERTIES (cache " " " ")
target_link_libraries (cache ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (cache PROPERTIES FOLDER test)

#-- Adding test for cache_api
add_executable (cache_api ${HDF5_TEST_SOURCE_DIR}/cache_api.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache_api ${LIB_TYPE})
TARGET_C_PROPERTIES (cache_api " " " ")
target_link_libraries (cache_api ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (cache_api PROPERTIES FOLDER test)

#-- Adding test for ttsafe
add_executable (ttsafe
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
)
TARGET_NAMING (ttsafe ${LIB_TYPE})
TARGET_C_PROPERTIES (ttsafe " " " ")
target_link_libraries (ttsafe ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (ttsafe PROPERTIES FOLDER test)

#-- Adding test for err_compat
if (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  add_executable (err_compat ${HDF5_TEST_SOURCE_DIR}/err_compat.c)
  TARGET_NAMING (err_compat ${LIB_TYPE})
  TARGET_C_PROPERTIES (err_compat " " " ")
  target_link_libraries (err_compat ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (err_compat PROPERTIES FOLDER test)
endif (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-- Adding test for error_test
add_executable (error_test ${HDF5_TEST_SOURCE_DIR}/error_test.c)
TARGET_NAMING (error_test ${LIB_TYPE})
TARGET_C_PROPERTIES (error_test " " " ")
target_link_libraries (error_test ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (error_test PROPERTIES FOLDER test)

#-- Adding test for links_env
add_executable (links_env ${HDF5_TEST_SOURCE_DIR}/links_env.c)
TARGET_NAMING (links_env ${LIB_TYPE})
TARGET_C_PROPERTIES (links_env " " " ")
target_link_libraries (links_env ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (links_env PROPERTIES FOLDER test)

#-- Adding test for libinfo
set (GREP_RUNNER ${PROJECT_BINARY_DIR}/GrepRunner.cmake)
file (WRITE ${GREP_RUNNER} 
  "file (STRINGS \${TEST_PROGRAM} TEST_RESULT REGEX \"SUMMARY OF THE HDF5 CONFIGURATION\")
if (\${TEST_RESULT} STREQUAL \"0\")
  message (FATAL_ERROR \"Failed: The output: \${TEST_RESULT} of \${TEST_PROGRAM} did not contain SUMMARY OF THE HDF5 CONFIGURATION\")
else (\${TEST_RESULT} STREQUAL \"0\")
  message (STATUS \"COMMAND Result: \${TEST_RESULT}\")
endif (\${TEST_RESULT} STREQUAL \"0\")
"
)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  add_executable (plugin ${HDF5_TEST_SOURCE_DIR}/plugin.c)
  TARGET_NAMING (plugin ${LIB_TYPE})
  TARGET_C_PROPERTIES (plugin " " " ")
  target_link_libraries (plugin ${HDF5_TEST_PLUGIN_LIB_TARGET})
  set_target_properties (plugin PROPERTIES FOLDER test)
endif (BUILD_SHARED_LIBS)

include (CMakeTests.cmake)
