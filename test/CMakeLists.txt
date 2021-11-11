cmake_minimum_required (VERSION 3.12)
project (HDF5_TEST C)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
set (srcdir ${HDF5_TEST_SOURCE_DIR})
configure_file (${HDF5_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)

#################################################################################
# Define Test Library Sources
#################################################################################
set (TEST_LIB_SOURCES
    ${HDF5_TEST_SOURCE_DIR}/h5test.c
    ${HDF5_TEST_SOURCE_DIR}/testframe.c
    ${HDF5_TEST_SOURCE_DIR}/cache_common.c
)

set (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
    ${HDF5_TEST_SOURCE_DIR}/H5srcdir.h
    ${HDF5_TEST_SOURCE_DIR}/cache_common.h
)

if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_TEST_LIB_TARGET} STATIC ${TEST_LIB_SOURCES} ${TEST_LIB_HEADERS})
  target_include_directories (${HDF5_TEST_LIB_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_TEST_LIB_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (${HDF5_TEST_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_TEST_LIB_TARGET}
      PUBLIC ${LINK_LIBS} ${HDF5_LIB_TARGET}
      PRIVATE $<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>
  )
  if (MINGW)
    target_link_libraries (${HDF5_TEST_LIB_TARGET} PRIVATE "wsock32.lib")
  endif ()
  H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_TEST_LIB_TARGET} PROPERTIES FOLDER libraries/test)
endif ()

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_TEST_LIBSH_TARGET} SHARED ${TEST_LIB_SOURCES} ${TEST_LIB_HEADERS})
  target_include_directories (${HDF5_TEST_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_TEST_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_TEST_LIBSH_TARGET} PUBLIC "H5_BUILT_AS_DYNAMIC_LIB")
  TARGET_C_PROPERTIES (${HDF5_TEST_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_TEST_LIBSH_TARGET}
      PUBLIC ${LINK_LIBS} ${HDF5_LIBSH_TARGET}
      PRIVATE $<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>
  )
  if (MINGW)
    target_link_libraries (${HDF5_TEST_LIBSH_TARGET} PRIVATE "wsock32.lib")
  endif ()
  H5_SET_LIB_OPTIONS (${HDF5_TEST_LIBSH_TARGET} ${HDF5_TEST_LIB_NAME} SHARED "LIB")
  set_target_properties (${HDF5_TEST_LIBSH_TARGET} PROPERTIES FOLDER libraries/test)
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_TEST_SRC_FORMAT ${HDF5_TEST_LIB_TARGET})
  else ()
    clang_format (HDF5_TEST_SRC_FORMAT ${HDF5_TEST_LIBSH_TARGET})
  endif ()
endif ()

#################################################################################
# If filter plugin tests can be tested
#################################################################################
if (BUILD_SHARED_LIBS)
  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir1")
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/testdir2")

  #-----------------------------------------------------------------------------
  # Define Filter Plugin Test Sources
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
    set (HDF5_TEST_PLUGIN_CORENAME         "${test_lib}")
    set (HDF5_TEST_PLUGIN_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_CORENAME}")
    set (HDF5_TEST_PLUGIN_TARGET           ${HDF5_TEST_PLUGIN_CORENAME})

    add_library (${HDF5_TEST_PLUGIN_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    target_include_directories (${HDF5_TEST_PLUGIN_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_TARGET} SHARED)
    target_link_libraries (${HDF5_TEST_PLUGIN_TARGET} PUBLIC ${HDF5_TEST_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_TEST_PLUGIN_TARGET} ${HDF5_TEST_PLUGIN_NAME} SHARED "LIB")
    set_target_properties (${HDF5_TEST_PLUGIN_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TEST_${HDF5_TEST_PLUGIN_TARGET}_FORMAT ${HDF5_TEST_PLUGIN_TARGET})
    endif ()

    #-----------------------------------------------------------------------------
    # Copy the filter plugin to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_TEST_PLUGIN_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
          "$<TARGET_FILE:${HDF5_TEST_PLUGIN_TARGET}>"
          "${CMAKE_BINARY_DIR}/testdir1/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_TARGET}>"
    )
  endforeach ()

  foreach (test_lib ${TEST2_PLUGIN_LIBS})
    set (HDF5_TEST_PLUGIN_CORENAME         "${test_lib}")
    set (HDF5_TEST_PLUGIN_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_CORENAME}")
    set (HDF5_TEST_PLUGIN_TARGET           ${HDF5_TEST_PLUGIN_CORENAME})

    add_library (${HDF5_TEST_PLUGIN_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${test_lib}.c)
    target_include_directories (${HDF5_TEST_PLUGIN_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${HDF5_TEST_PLUGIN_TARGET} SHARED)
    target_link_libraries (${HDF5_TEST_PLUGIN_TARGET} PUBLIC ${HDF5_TEST_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_TEST_PLUGIN_TARGET} ${HDF5_TEST_PLUGIN_NAME} SHARED "LIB")
    set_target_properties (${HDF5_TEST_PLUGIN_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TEST_${HDF5_TEST_PLUGIN_TARGET}_FORMAT ${HDF5_TEST_PLUGIN_TARGET})
    endif ()

    #-----------------------------------------------------------------------------
    # Copy the filter plugin to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_TEST_PLUGIN_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
          "$<TARGET_FILE:${HDF5_TEST_PLUGIN_TARGET}>"
          "${CMAKE_BINARY_DIR}/testdir2/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_TARGET}>"
    )
  endforeach ()
endif ()

#################################################################################
#  Test program sources
#################################################################################

set (testhdf5_SOURCES
    ${HDF5_TEST_SOURCE_DIR}/testhdf5.h
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
    ${HDF5_TEST_SOURCE_DIR}/tverbounds18.c
    ${HDF5_TEST_SOURCE_DIR}/tvltypes.c
    ${HDF5_TEST_SOURCE_DIR}/tvlstr.c
)

set (ttsafe_SOURCES
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.h
    ${HDF5_TEST_SOURCE_DIR}/ttsafe.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_dcreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_error.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_cancel.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_acreate.c
    ${HDF5_TEST_SOURCE_DIR}/ttsafe_attr_vlen.c
)

set (H5_TESTS
    testhdf5 # multiple source
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
    big
    mtime
    fillval
    mount
    flush1
    flush2
    app_ref
    enum
    set_extent
    ttsafe # multiple source
    getname
    vfd
    ros3
    s3comms
    hdfs
    ntypes
    dangle
    dtransform
    # reserved disabled
    cross_read
    freespace
    mf
    btree2
    fheap
    file_image
    unregister
)

macro (ADD_H5_EXE file)
  add_executable (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  target_include_directories (${file} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(${file} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${file} STATIC)
    target_link_libraries (${file} PRIVATE ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (${file} SHARED)
    target_link_libraries (${file} PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (${file} PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_${file}_FORMAT ${file})
  endif ()
endmacro ()

set (H5_TESTS_MULTIPLE
    testhdf5
    ttsafe
)
# Only build single source tests here
foreach (h5_test ${H5_TESTS})
  if (NOT h5_test IN_LIST H5_TESTS_MULTIPLE)
    ADD_H5_EXE(${h5_test})
  endif ()
endforeach ()

##############################################################################
###           M U L T I P L E  S O U R C E   T E S T S                     ###
##############################################################################


######### Special handling for multiple sources #############
#-- Adding test for testhdf5
add_executable (testhdf5 ${testhdf5_SOURCES})
target_compile_options(testhdf5 PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_include_directories (testhdf5 PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (testhdf5 STATIC)
  target_link_libraries (testhdf5 PRIVATE ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (testhdf5 SHARED)
  target_link_libraries (testhdf5 PRIVATE ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (testhdf5 PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_testhdf5_FORMAT testhdf5)
endif ()

#-- Adding test for ttsafe
add_executable (ttsafe ${ttsafe_SOURCES})
target_compile_options(ttsafe PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_include_directories (ttsafe PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (ttsafe STATIC)
  target_link_libraries (ttsafe PRIVATE ${HDF5_TEST_LIB_TARGET})
  if (NOT WIN32)
    target_link_libraries (ttsafe
      PRIVATE $<$<BOOL:${HDF5_ENABLE_THREADSAFE}>:Threads::Threads>
    )
  endif ()
else ()
  TARGET_C_PROPERTIES (ttsafe SHARED)
  target_link_libraries (ttsafe PRIVATE ${HDF5_TEST_LIBSH_TARGET} $<$<BOOL:${HDF5_ENABLE_THREADSAFE}>:Threads::Threads>)
endif ()
set_target_properties (ttsafe PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_ttsafe_FORMAT ttsafe)
endif ()

##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################

set (H5_CHECK_TESTS
    error_test
    err_compat
    tcheck_version
    testmeta
    links_env
)

foreach (h5_test ${H5_CHECK_TESTS})
  ADD_H5_EXE(${h5_test})
endforeach ()

#-- Adding test for libinfo
set (GREP_RUNNER ${PROJECT_BINARY_DIR}/GrepRunner.cmake)
file (WRITE ${GREP_RUNNER}
  "file (STRINGS \${TEST_PROGRAM} TEST_RESULT REGEX \"SUMMARY OF THE HDF5 CONFIGURATION\")
if (NOT TEST_RESULT)
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
  add_executable (filter_plugin ${HDF5_TEST_SOURCE_DIR}/plugin.c)
  target_include_directories (filter_plugin PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (filter_plugin SHARED)
  target_link_libraries (filter_plugin PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  set_target_properties (filter_plugin PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_filter_plugin_FORMAT filter_plugin)
  endif ()
endif ()

if (HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
