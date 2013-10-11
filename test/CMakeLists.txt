cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TEST)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

#-----------------------------------------------------------------------------
# Define Test Library Sources
#-----------------------------------------------------------------------------
SET (TEST_LIB_SRCS
    ${HDF5_TEST_SOURCE_DIR}/h5test.c
    ${HDF5_TEST_SOURCE_DIR}/testframe.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
)

SET (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
)

ADD_LIBRARY (${HDF5_TEST_LIB_TARGET} ${LIB_TYPE} ${TEST_LIB_SRCS} ${TEST_LIB_HEADERS})
TARGET_C_PROPERTIES (${HDF5_TEST_LIB_TARGET} " " " ")
IF (MSVC)
  TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "ws2_32.lib")
ENDIF (MSVC)
IF (MINGW)
  TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "wsock32.lib")
ENDIF (MINGW)
TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} ${LIB_TYPE})
SET_TARGET_PROPERTIES (${HDF5_TEST_LIB_TARGET} PROPERTIES FOLDER libraries/test)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
IF (BUILD_SHARED_LIBS)
  # make plugins dir
  FILE (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir1")
  FILE (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir2")

  #-----------------------------------------------------------------------------
  # Define Test Library Sources
  #-----------------------------------------------------------------------------
  SET (TEST_PLUGIN_LIBS
      dynlib1
      dynlib3
  )
  SET (TEST2_PLUGIN_LIBS
      dynlib2
  )

  FOREACH (test_lib ${TEST_PLUGIN_LIBS})
    SET (HDF5_TEST_PLUGIN_LIB_CORENAME         "${test_lib}")
    SET (HDF5_TEST_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")
    SET (HDF5_TEST_PLUGIN_LIB_TARGET           ${HDF5_TEST_PLUGIN_LIB_CORENAME})
    ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    ADD_LIBRARY (${HDF5_TEST_PLUGIN_LIB_TARGET} ${LIB_TYPE} ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} " " " ")
    TARGET_LINK_LIBRARIES (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (
        ${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME}
        ${LIB_TYPE}
        HDF5_TEST_PLUGIN_LIB_NAME_RELEASE
        HDF5_TEST_PLUGIN_LIB_NAME_DEBUG
    )
    SET_TARGET_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    ADD_CUSTOM_COMMAND (
        TARGET     ${HDF5_TEST_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different 
          "$<TARGET_FILE:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/testdir1/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
    )
  ENDFOREACH (test_lib ${TEST_PLUGIN_LIBS})

  FOREACH (test_lib ${TEST2_PLUGIN_LIBS})
    SET (HDF5_TEST_PLUGIN_LIB_CORENAME         "${test_lib}")
    SET (HDF5_TEST_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")
    SET (HDF5_TEST_PLUGIN_LIB_TARGET           ${HDF5_TEST_PLUGIN_LIB_CORENAME})
    ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    ADD_LIBRARY (${HDF5_TEST_PLUGIN_LIB_TARGET} ${LIB_TYPE} ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} " " " ")
    TARGET_LINK_LIBRARIES (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (
        ${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME}
        ${LIB_TYPE}
        HDF5_TEST_PLUGIN_LIB_NAME_RELEASE
        HDF5_TEST_PLUGIN_LIB_NAME_DEBUG
    )
    SET_TARGET_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    ADD_CUSTOM_COMMAND (
        TARGET     ${HDF5_TEST_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different 
          "$<TARGET_FILE:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/testdir2/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_LIB_TARGET}>"
    )
  ENDFOREACH (test_lib ${TEST2_PLUGIN_LIBS})
ENDIF (BUILD_SHARED_LIBS)

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
TARGET_C_PROPERTIES (testhdf5 " " " ")
TARGET_LINK_LIBRARIES (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (testhdf5 PROPERTIES FOLDER test)

MACRO (ADD_H5_EXE file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_C_PROPERTIES (${file} " " " ")
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (${file} PROPERTIES FOLDER test)
ENDMACRO (ADD_H5_EXE file)

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
    unregister
)

FOREACH (test ${H5_TESTS})
  ADD_H5_EXE(${test})
ENDFOREACH (test ${H5_TESTS})

##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
ADD_EXECUTABLE (cache ${HDF5_TEST_SOURCE_DIR}/cache.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache ${LIB_TYPE})
TARGET_C_PROPERTIES (cache " " " ")
TARGET_LINK_LIBRARIES (cache ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (cache PROPERTIES FOLDER test)

#-- Adding test for cache_api
ADD_EXECUTABLE (cache_api ${HDF5_TEST_SOURCE_DIR}/cache_api.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache_api ${LIB_TYPE})
TARGET_C_PROPERTIES (cache_api " " " ")
TARGET_LINK_LIBRARIES (cache_api ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (cache_api PROPERTIES FOLDER test)

#-- Adding test for cache_tagging
ADD_EXECUTABLE (cache_tagging ${HDF5_TEST_SOURCE_DIR}/cache_tagging.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
TARGET_NAMING (cache_tagging ${LIB_TYPE})
TARGET_C_PROPERTIES (cache_tagging " " " ")
TARGET_LINK_LIBRARIES (cache_tagging ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (cache_tagging PROPERTIES FOLDER test)

#-- Adding test for ttsafe
ADD_EXECUTABLE (ttsafe
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
)
TARGET_NAMING (ttsafe ${LIB_TYPE})
TARGET_C_PROPERTIES (ttsafe " " " ")
TARGET_LINK_LIBRARIES (ttsafe ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (ttsafe PROPERTIES FOLDER test)

#-- Adding test for err_compat
IF (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  ADD_EXECUTABLE (err_compat ${HDF5_TEST_SOURCE_DIR}/err_compat.c)
  TARGET_NAMING (err_compat ${LIB_TYPE})
  TARGET_C_PROPERTIES (err_compat " " " ")
  TARGET_LINK_LIBRARIES (err_compat ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  SET_TARGET_PROPERTIES (err_compat PROPERTIES FOLDER test)
ENDIF (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-- Adding test for error_test
ADD_EXECUTABLE (error_test ${HDF5_TEST_SOURCE_DIR}/error_test.c)
TARGET_NAMING (error_test ${LIB_TYPE})
TARGET_C_PROPERTIES (error_test " " " ")
TARGET_LINK_LIBRARIES (error_test ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (error_test PROPERTIES FOLDER test)

#-- Adding test for links_env
ADD_EXECUTABLE (links_env ${HDF5_TEST_SOURCE_DIR}/links_env.c)
TARGET_NAMING (links_env ${LIB_TYPE})
TARGET_C_PROPERTIES (links_env " " " ")
TARGET_LINK_LIBRARIES (links_env ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
SET_TARGET_PROPERTIES (links_env PROPERTIES FOLDER test)

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

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
IF (BUILD_SHARED_LIBS)
  ADD_EXECUTABLE (plugin ${HDF5_TEST_SOURCE_DIR}/plugin.c)
  TARGET_NAMING (plugin ${LIB_TYPE})
  TARGET_C_PROPERTIES (plugin " " " ")
  TARGET_LINK_LIBRARIES (plugin ${HDF5_TEST_PLUGIN_LIB_TARGET})
  SET_TARGET_PROPERTIES (plugin PROPERTIES FOLDER test)
ENDIF (BUILD_SHARED_LIBS)

INCLUDE (CMakeTests.cmake)
