cmake_minimum_required (VERSION 2.8)
PROJECT (H5_TEST)

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
SET (srdir ${CMAKE_CURRENT_SOURCE_DIR})
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

SET (testhdf5_SRCS
    ${HDF5_TEST_SOURCE_DIR}/testhdf5.c
    ${HDF5_TEST_SOURCE_DIR}/tarray.c
    ${HDF5_TEST_SOURCE_DIR}/tattr.c
    ${HDF5_TEST_SOURCE_DIR}/tchecksum.c
    ${HDF5_TEST_SOURCE_DIR}/tconfig.c
    ${HDF5_TEST_SOURCE_DIR}/tcoords.c
    ${HDF5_TEST_SOURCE_DIR}/tfile.c
    ${HDF5_TEST_SOURCE_DIR}/tgenprop.c
    ${HDF5_TEST_SOURCE_DIR}/th5s.c
    ${HDF5_TEST_SOURCE_DIR}/theap.c
    ${HDF5_TEST_SOURCE_DIR}/titerate.c
    ${HDF5_TEST_SOURCE_DIR}/tmeta.c
    ${HDF5_TEST_SOURCE_DIR}/tmisc.c
    ${HDF5_TEST_SOURCE_DIR}/trefer.c
    ${HDF5_TEST_SOURCE_DIR}/trefstr.c
    ${HDF5_TEST_SOURCE_DIR}/tselect.c
    ${HDF5_TEST_SOURCE_DIR}/tskiplist.c
    ${HDF5_TEST_SOURCE_DIR}/ttime.c
    ${HDF5_TEST_SOURCE_DIR}/ttst.c
    ${HDF5_TEST_SOURCE_DIR}/tunicode.c
    ${HDF5_TEST_SOURCE_DIR}/tvltypes.c
    ${HDF5_TEST_SOURCE_DIR}/tvlstr.c
    ${HDF5_TEST_SOURCE_DIR}/th5o.c
    ${HDF5_TEST_SOURCE_DIR}/tsohm.c
    ${HDF5_TEST_SOURCE_DIR}/tid.c
)

#-- Adding test for testhdf5
ADD_EXECUTABLE (testhdf5 ${testhdf5_SRCS})
H5_NAMING (testhdf5)
TARGET_LINK_LIBRARIES (testhdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_TEST (NAME testhdf5 COMMAND $<TARGET_FILE:testhdf5>)

MACRO (ADD_H5_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  H5_NAMING (${file})
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

  ADD_TEST (NAME ${file} COMMAND $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5_TEST file)

SET (H5_TESTS
    app_ref
    big
    bittests
    btree2
#    cache
#    cache_api
    cmpd_dset
    cross_read
    dangle
    dsets
    dt_arith
    dtransform
    dtypes
    enum
    err_compat
    error_test
    extend
    external
    fheap
    fillval
    flush1
    flush2
    freespace
    getname
    gheap
    hyperslab
    istore
    lheap
    links
    mf
    mount
    mtime
    ntypes
    objcopy
    ohdr
    pool
    reserved
    set_extent
    stab
    tcheck_version
    testmeta
    ttsafe
    unlink
    vfd
)

FOREACH (test ${H5_TESTS})
    ADD_H5_TEST(${test})
ENDFOREACH (test ${H5_TESTS})

#-- Allow extra time for fheap to complete 30min
IF (WIN32)
  SET_TESTS_PROPERTIES (fheap PROPERTIES TIMEOUT 1800)
ENDIF (WIN32)


#-- Adding test for cache
ADD_EXECUTABLE (cache ${HDF5_TEST_SOURCE_DIR}/cache.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
H5_NAMING (cache)
TARGET_LINK_LIBRARIES (cache ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache COMMAND $<TARGET_FILE:cache>)

ADD_EXECUTABLE (cache_api ${HDF5_TEST_SOURCE_DIR}/cache_api.c ${HDF5_TEST_SOURCE_DIR}/cache_common.c)
H5_NAMING (cache_api)
TARGET_LINK_LIBRARIES (cache_api ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})

ADD_TEST (NAME cache_api COMMAND $<TARGET_FILE:cache_api>)

#-- Copy all the HDF5 files from the test directory into the source directory
SET (HDF5_REFERENCE_TEST_FILES
  tnullspace.h5
)

FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
  #MESSAGE (STATUS " Copying ${h5_file}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/${h5_file} ${dest}
  )
ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})


#-- Copy all the HDF5 files from the test directory into the source directory
SET (HDF5_REFERENCE_TEST_FILES
    mergemsg.h5
    tbogus.h5
    tbad_msg_count.h5
    deflate.h5
    noencoder.h5
    tmtimeo.h5
    tmtimen.h5
    fill_old.h5
    tlayouto.h5
    th5s.h5
    tarrold.h5
    group_old.h5
    corrupt_stab_msg.h5
    fixed_idx.h5
    le_extlink2.h5
    be_extlink2.h5
    le_extlink1.h5
    be_extlink1.h5
    specmetaread.h5
    test_filters_le.hdf5
    test_filters_be.hdf5
    vms_data.h5
    le_data.h5
    be_data.h5
    family_v16_00000.h5
    family_v16_00001.h5
    family_v16_00002.h5
    family_v16_00003.h5
)

FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  SET (dest "${H5_TEST_BINARY_DIR}/${h5_file}")
  #MESSAGE (STATUS " Copying ${h5_file} to ${dest}")
  ADD_CUSTOM_COMMAND (
      TARGET     ${HDF5_TEST_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${H5_TEST_SOURCE_DIR}/${h5_file} ${dest}
  )
ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

