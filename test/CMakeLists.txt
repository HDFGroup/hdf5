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

INCLUDE (CMakeTests.cmake)
