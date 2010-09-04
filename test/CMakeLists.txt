cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TEST)

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
IF (BUILD_SHARED_LIBS)
  IF (MSVC)
    TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} "ws2_32.lib")
  ENDIF (MSVC)
ENDIF (BUILD_SHARED_LIBS)
TARGET_LINK_LIBRARIES (${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TEST_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} ${LIB_TYPE})

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
)

FOREACH (ref_file ${HDF5_REFERENCE_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/testfiles/${ref_file}")
  #MESSAGE (STATUS " Copying ${h5_file}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TEST_SOURCE_DIR}/testfiles/${ref_file} ${dest}
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
    filespace_1_6.h5
    filespace_1_8.h5
    fill_old.h5
    group_old.h5
    le_data.h5
    le_extlink1.h5
    le_extlink2.h5
    mergemsg.h5
    noencoder.h5
    specmetaread.h5
    tarrold.h5
    tbad_msg_count.h5
    tbogus.h5
    test_filters_be.hdf5
    test_filters_le.hdf5
    th5s.h5
    tlayouto.h5
    tmtimen.h5
    tmtimeo.h5
    tsizeslheap.h5
    vms_data.h5
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
H5_NAMING (testhdf5)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (testhdf5
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_TEST (NAME testhdf5 COMMAND $<TARGET_FILE:testhdf5>)
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

MACRO (ADD_H5_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  H5_NAMING (${file})
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (${file}
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

  ADD_TEST (NAME ${file} COMMAND $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5_TEST file)

SET (H5_TESTS
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
    extend
    external
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
    error_test
    err_compat
    tcheck_version
    testmeta
)

FOREACH (test ${H5_TESTS})
    ADD_H5_TEST(${test})
ENDFOREACH (test ${H5_TESTS})

#-- Allow extra time for fheap to complete 30min
IF (WIN32)
  SET_TESTS_PROPERTIES (fheap PROPERTIES TIMEOUT 2500)
ENDIF (WIN32)


##############################################################################
##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################
##############################################################################

#-- Adding test for cache
ADD_EXECUTABLE (cache ${HDF5_TEST_SOURCE_DIR}/cache.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
H5_NAMING (cache)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (cache
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (cache ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache COMMAND $<TARGET_FILE:cache>)

#-- Adding test for cache_api
ADD_EXECUTABLE (cache_api ${HDF5_TEST_SOURCE_DIR}/cache_api.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
H5_NAMING (cache_api)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (cache_api
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (cache_api ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache_api COMMAND $<TARGET_FILE:cache_api>)

#-- Adding test for cache_tagging
ADD_EXECUTABLE (cache_tagging ${HDF5_TEST_SOURCE_DIR}/cache_tagging.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
H5_NAMING (cache_tagging)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (cache_tagging
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (cache_tagging ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache_tagging COMMAND $<TARGET_FILE:cache_tagging>)
#-- Adding test for ttsafe
ADD_EXECUTABLE (ttsafe
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
)
H5_NAMING (ttsafe)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (ttsafe
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (ttsafe ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME ttsafe COMMAND $<TARGET_FILE:ttsafe>)

##############################################################################
##############################################################################
###           T H E   G E N E R A T O R S                                  ###
##############################################################################
##############################################################################

IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  MACRO (ADD_H5_GENERATOR genfile)
    ADD_EXECUTABLE (${genfile} ${HDF5_TEST_SOURCE_DIR}/${genfile}.c)
    H5_NAMING (${genfile})
    IF (WIN32)
      IF (MSVC)
        IF (NOT BUILD_SHARED_LIBS)
          SET_TARGET_PROPERTIES (${genfile}
              PROPERTIES
                  LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
          ) 
        ENDIF (NOT BUILD_SHARED_LIBS)
      ENDIF (MSVC)
    ENDIF (WIN32)
    TARGET_LINK_LIBRARIES (${genfile} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
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
  )

  FOREACH (gen ${H5_GENERATORS})
    ADD_EXECUTABLE (${gen} ${HDF5_TEST_SOURCE_DIR}/${gen}.c)
    H5_NAMING (${gen})
    TARGET_LINK_LIBRARIES (${gen} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  ENDFOREACH (gen ${H5_GENERATORS})

ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

