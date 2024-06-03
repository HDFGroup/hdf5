cmake_minimum_required (VERSION 3.18)
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
    ${HDF5_TEST_SOURCE_DIR}/external_common.c
    ${HDF5_TEST_SOURCE_DIR}/swmr_common.c
)

set (TEST_LIB_HEADERS
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
    ${HDF5_TEST_SOURCE_DIR}/H5srcdir.h
    ${HDF5_TEST_SOURCE_DIR}/cache_common.h
    ${HDF5_TEST_SOURCE_DIR}/external_common.h
    ${HDF5_TEST_SOURCE_DIR}/external_fname.h
    ${HDF5_TEST_SOURCE_DIR}/swmr_common.h
)

#################################################################################
# Set private compile-time definitions added when
# compiling test source files
#################################################################################
set (HDF5_TEST_COMPILE_DEFS_PRIVATE
    "$<$<CONFIG:Developer>:${HDF5_DEVELOPER_DEFS}>"
    "$<$<BOOL:HDF5_TEST_API>:H5_HAVE_TEST_API>"
)

if (BUILD_STATIC_LIBS)
  add_library (${HDF5_TEST_LIB_TARGET} STATIC ${TEST_LIB_SOURCES} ${TEST_LIB_HEADERS})
  target_include_directories (${HDF5_TEST_LIB_TARGET}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_TEST_LIB_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_TEST_LIB_TARGET}
      PRIVATE
          "H5_TEST_EXPRESS_LEVEL_DEFAULT=${H5_TEST_EXPRESS_LEVEL_DEFAULT}"
          "${HDF5_TEST_COMPILE_DEFS_PRIVATE}"
  )
  TARGET_C_PROPERTIES (${HDF5_TEST_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_TEST_LIB_TARGET}
      PUBLIC ${LINK_LIBS} ${HDF5_LIB_TARGET}
      PRIVATE "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
  )
  if (MINGW)
    target_link_libraries (${HDF5_TEST_LIB_TARGET} PRIVATE "wsock32.lib")
  endif ()
  H5_SET_LIB_OPTIONS (${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_TEST_LIB_TARGET} PROPERTIES FOLDER libraries/test)

  if (HDF5_EXPORTED_TARGETS AND HDF5_TEST_API_INSTALL)
    INSTALL_TARGET_PDB (${HDF5_TEST_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} libraries)

    install (
      TARGETS ${HDF5_TEST_LIB_TARGET}
      EXPORT ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT libraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT libraries
      INCLUDES DESTINATION include
    )
  endif ()
endif ()

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_TEST_LIBSH_TARGET} SHARED ${TEST_LIB_SOURCES} ${TEST_LIB_HEADERS})
  target_include_directories (${HDF5_TEST_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_TEST_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_TEST_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          "H5_TEST_EXPRESS_LEVEL_DEFAULT=${H5_TEST_EXPRESS_LEVEL_DEFAULT}"
          "${HDF5_TEST_COMPILE_DEFS_PRIVATE}"
  )
  TARGET_C_PROPERTIES (${HDF5_TEST_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_TEST_LIBSH_TARGET}
      PUBLIC ${LINK_LIBS} ${HDF5_LIBSH_TARGET}
      PRIVATE "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
  )
  if (MINGW)
    target_link_libraries (${HDF5_TEST_LIBSH_TARGET} PRIVATE "wsock32.lib")
  endif ()
  H5_SET_LIB_OPTIONS (${HDF5_TEST_LIBSH_TARGET} ${HDF5_TEST_LIB_NAME} SHARED "LIB")
  set_target_properties (${HDF5_TEST_LIBSH_TARGET} PROPERTIES FOLDER libraries/test)

  if (HDF5_EXPORTED_TARGETS AND HDF5_TEST_API_INSTALL)
    INSTALL_TARGET_PDB (${HDF5_TEST_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} libraries)

    install (
      TARGETS ${HDF5_TEST_LIBSH_TARGET}
      EXPORT ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT libraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT libraries
      INCLUDES DESTINATION include
    )
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (BUILD_STATIC_LIBS)
    clang_format (HDF5_TEST_SRC_FORMAT ${HDF5_TEST_LIB_TARGET})
  else ()
    clang_format (HDF5_TEST_SRC_FORMAT ${HDF5_TEST_LIBSH_TARGET})
  endif ()
endif ()

#################################################################################
# If filter, vfd and vol plugin tests can be tested
#################################################################################
if (BUILD_SHARED_LIBS)
  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/filter_plugin_dir1")
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/filter_plugin_dir2")

  #-----------------------------------------------------------------------------
  # Define Filter Plugin Test Sources
  #-----------------------------------------------------------------------------
  set (FILTER_PLUGINS_FOR_DIR1
      filter_plugin1_dsets
      filter_plugin3_dsets
  )
  set (FILTER_PLUGINS_FOR_DIR2
      filter_plugin2_dsets
      filter_plugin4_groups
  )

  foreach (plugin_name ${FILTER_PLUGINS_FOR_DIR1})
    set (HDF5_TEST_PLUGIN_CORENAME  "${plugin_name}")
    set (HDF5_TEST_PLUGIN_NAME      "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_CORENAME}")
    set (HDF5_TEST_PLUGIN_TARGET    ${HDF5_TEST_PLUGIN_CORENAME})

    add_library (${HDF5_TEST_PLUGIN_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${plugin_name}.c)
    target_include_directories (${HDF5_TEST_PLUGIN_TARGET} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
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
          "${CMAKE_BINARY_DIR}/filter_plugin_dir1/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_TARGET}>"
    )
  endforeach ()

  foreach (plugin_name ${FILTER_PLUGINS_FOR_DIR2})
    set (HDF5_TEST_PLUGIN_CORENAME  "${plugin_name}")
    set (HDF5_TEST_PLUGIN_NAME      "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_CORENAME}")
    set (HDF5_TEST_PLUGIN_TARGET    ${HDF5_TEST_PLUGIN_CORENAME})

    add_library (${HDF5_TEST_PLUGIN_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${plugin_name}.c)
    target_include_directories (${HDF5_TEST_PLUGIN_TARGET} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
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
          "${CMAKE_BINARY_DIR}/filter_plugin_dir2/$<TARGET_FILE_NAME:${HDF5_TEST_PLUGIN_TARGET}>"
    )
  endforeach ()

  #################################################################################
  # make vfd plugins dir
  #################################################################################
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/null_vfd_plugin_dir")

  #-----------------------------------------------------------------------------
  # Define VFD Plugin Test Sources
  #-----------------------------------------------------------------------------
  set (VFD_PLUGIN_LIBS
      null_vfd_plugin
  )

  foreach (vfd_lib ${VFD_PLUGIN_LIBS})
    set (HDF5_VFD_PLUGIN_LIB_CORENAME  "${vfd_lib}")
    set (HDF5_VFD_PLUGIN_LIB_NAME      "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_VFD_PLUGIN_LIB_CORENAME}")
    set (HDF5_VFD_PLUGIN_LIB_TARGET    ${HDF5_VFD_PLUGIN_LIB_CORENAME})

    add_library (${HDF5_VFD_PLUGIN_LIB_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${vfd_lib}.c)
    target_include_directories (${HDF5_VFD_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${HDF5_VFD_PLUGIN_LIB_TARGET} SHARED)
    target_link_libraries (${HDF5_VFD_PLUGIN_LIB_TARGET} PUBLIC ${HDF5_TEST_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_VFD_PLUGIN_LIB_TARGET} ${HDF5_VFD_PLUGIN_LIB_NAME} SHARED "LIB")
    set_target_properties (${HDF5_VFD_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TEST_${HDF5_VFD_PLUGIN_LIB_TARGET}_FORMAT ${HDF5_VFD_PLUGIN_LIB_TARGET})
    endif ()

    #-----------------------------------------------------------------------------
    # Copy VFD plugin to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_VFD_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
          "$<TARGET_FILE:${HDF5_VFD_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/null_vfd_plugin_dir/$<TARGET_FILE_NAME:${HDF5_VFD_PLUGIN_LIB_TARGET}>"
    )
  endforeach ()

  #################################################################################
  # make vol plugins dir
  #################################################################################
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/null_vol_plugin_dir")

  #-----------------------------------------------------------------------------
  # Define VOL Plugin Test Sources
  #-----------------------------------------------------------------------------
  set (VOL_PLUGIN_LIBS
      null_vol_connector
  )

  foreach (vol_lib ${VOL_PLUGIN_LIBS})
    set (HDF5_VOL_PLUGIN_LIB_CORENAME  "${vol_lib}")
    set (HDF5_VOL_PLUGIN_LIB_NAME      "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_VOL_PLUGIN_LIB_CORENAME}")
    set (HDF5_VOL_PLUGIN_LIB_TARGET    ${HDF5_VOL_PLUGIN_LIB_CORENAME})

    add_library (${HDF5_VOL_PLUGIN_LIB_TARGET} SHARED ${HDF5_TEST_SOURCE_DIR}/${vol_lib}.c)
    target_include_directories (${HDF5_VOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${HDF5_VOL_PLUGIN_LIB_TARGET} SHARED)
    target_link_libraries (${HDF5_VOL_PLUGIN_LIB_TARGET} PUBLIC ${HDF5_TEST_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_VOL_PLUGIN_LIB_TARGET} ${HDF5_VOL_PLUGIN_LIB_NAME} SHARED "LIB")
    set_target_properties (${HDF5_VOL_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TEST_${HDF5_VOL_PLUGIN_LIB_TARGET}_FORMAT ${HDF5_VOL_PLUGIN_LIB_TARGET})
    endif ()

    #-----------------------------------------------------------------------------
    # Copy VOL plugin to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_VOL_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
          "$<TARGET_FILE:${HDF5_VOL_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/null_vol_plugin_dir/$<TARGET_FILE_NAME:${HDF5_VOL_PLUGIN_LIB_TARGET}>"
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
    ${HDF5_TEST_SOURCE_DIR}/th5_system.c
    ${HDF5_TEST_SOURCE_DIR}/tcoords.c
    ${HDF5_TEST_SOURCE_DIR}/tfile.c
    ${HDF5_TEST_SOURCE_DIR}/tgenprop.c
    ${HDF5_TEST_SOURCE_DIR}/th5o.c
    ${HDF5_TEST_SOURCE_DIR}/th5s.c
    ${HDF5_TEST_SOURCE_DIR}/tid.c
    ${HDF5_TEST_SOURCE_DIR}/titerate.c
    ${HDF5_TEST_SOURCE_DIR}/tmeta.c
    ${HDF5_TEST_SOURCE_DIR}/tmisc.c
    ${HDF5_TEST_SOURCE_DIR}/trefer.c
    ${HDF5_TEST_SOURCE_DIR}/trefer_deprec.c
    ${HDF5_TEST_SOURCE_DIR}/trefstr.c
    ${HDF5_TEST_SOURCE_DIR}/tselect.c
    ${HDF5_TEST_SOURCE_DIR}/tskiplist.c
    ${HDF5_TEST_SOURCE_DIR}/tsohm.c
    ${HDF5_TEST_SOURCE_DIR}/ttime.c
    ${HDF5_TEST_SOURCE_DIR}/tunicode.c
    ${HDF5_TEST_SOURCE_DIR}/tvltypes.c
    ${HDF5_TEST_SOURCE_DIR}/tvlstr.c
)

set (cache_image_SOURCES
    ${HDF5_TEST_SOURCE_DIR}/cache_image.c
    ${HDF5_TEST_SOURCE_DIR}/genall5.c
)

set(mirror_vfd_SOURCES
    ${HDF5_TEST_SOURCE_DIR}/mirror_vfd.c
    ${HDF5_TEST_SOURCE_DIR}/genall5.c
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

set (H5_EXPRESS_TESTS
    cache
    cache_api
    cache_image # multiple source
    btree2
    farray
    earray
    fheap
    objcopy_ref
    objcopy
)

set (H5_TESTS
    testhdf5 # multiple source
    cache_tagging
    lheap
    ohdr
    stab
    gheap
    evict_on_close
    accum
    hyperslab
    istore
    bittests
    dt_arith
    page_buffer
    dtypes
    dsets
    select_io_dset
    chunk_info # compression lib link
    cmpd_dset
    mdset
    filter_fail
    extend
    direct_chunk # compression lib link
    external
    external_env
    efc
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
    ttsafe # multiple source
    enc_dec_plist
    enc_dec_plist_cross_platform
    getname
    vfd
    ros3
    s3comms
    hdfs
    #mirror_vfd # multiple source
    ntypes
    dangle
    dtransform
    # reserved disabled
    cross_read
    freespace
    mf
    vds
    vds_env
    file_image
    unregister
    cache_logging
    cork
    swmr
    thread_id # special link
    vol
    timer
    cmpd_dtransform
    event_set
    onion
)
if (HDF5_BUILD_UTILS)
  set (H5_TESTS ${H5_TESTS} mirror_vfd)
endif ()

macro (ADD_H5_EXE file)
  add_executable (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c)
  target_include_directories (${file} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(${file} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${file} PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
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
    chunk_info
    direct_chunk
    testhdf5
    cache_image
    ttsafe
    thread_id # special link
    mirror_vfd
)
# Only build single source tests here
foreach (h5_test ${H5_TESTS} ${H5_EXPRESS_TESTS})
  if (NOT h5_test IN_LIST H5_TESTS_MULTIPLE)
    ADD_H5_EXE(${h5_test})
  endif ()
endforeach ()

##############################################################################
###           M U L T I P L E  S O U R C E   T E S T S                     ###
##############################################################################
######### Also special handling of link libs #############
#-- Adding test for chunk_info
add_executable (chunk_info ${HDF5_TEST_SOURCE_DIR}/chunk_info.c)
target_compile_options(chunk_info PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(chunk_info PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (chunk_info PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (chunk_info STATIC)
  target_link_libraries (chunk_info PRIVATE ${HDF5_TEST_LIB_TARGET} ${LINK_COMP_LIBS})
else ()
  TARGET_C_PROPERTIES (chunk_info SHARED)
  target_link_libraries (chunk_info PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${LINK_COMP_LIBS})
endif ()
set_target_properties (chunk_info PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_SRC_chunk_info_FORMAT chunk_info)
endif ()

#-- Adding test for direct_chunk
add_executable (direct_chunk ${HDF5_TEST_SOURCE_DIR}/direct_chunk.c)
target_compile_options(direct_chunk PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(direct_chunk PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (direct_chunk PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (direct_chunk STATIC)
  target_link_libraries (direct_chunk PRIVATE ${HDF5_TEST_LIB_TARGET} ${LINK_COMP_LIBS})
else ()
  TARGET_C_PROPERTIES (direct_chunk SHARED)
  target_link_libraries (direct_chunk PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${LINK_COMP_LIBS})
endif ()
set_target_properties (direct_chunk PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_direct_chunk_FORMAT direct_chunk)
endif ()

######### Special handling for multiple sources #############
#-- Adding test for testhdf5
add_executable (testhdf5 ${testhdf5_SOURCES})
target_compile_options(testhdf5 PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(testhdf5 PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (testhdf5 PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
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

#-- Adding test for cache_image
add_executable (cache_image ${cache_image_SOURCES})
target_compile_options(cache_image PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(cache_image PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (cache_image PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (cache_image STATIC)
  target_link_libraries (cache_image PRIVATE ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (cache_image SHARED)
  target_link_libraries (cache_image PRIVATE ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (cache_image PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_cache_image_FORMAT cache_image)
endif ()

#-- Adding test for ttsafe
add_executable (ttsafe ${ttsafe_SOURCES})
target_compile_options(ttsafe PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(ttsafe PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (ttsafe PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (ttsafe STATIC)
  target_link_libraries (ttsafe PRIVATE ${HDF5_TEST_LIB_TARGET})
  if (NOT WIN32)
    target_link_libraries (ttsafe PRIVATE "$<$<BOOL:${HDF5_ENABLE_THREADSAFE}>:Threads::Threads>")
  endif ()
else ()
  TARGET_C_PROPERTIES (ttsafe SHARED)
  target_link_libraries (ttsafe PRIVATE ${HDF5_TEST_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_THREADSAFE}>:Threads::Threads>")
endif ()
set_target_properties (ttsafe PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_ttsafe_FORMAT ttsafe)
endif ()

######### Special handling for extra link lib of threads #############
#-- Adding test for thread_id
add_executable (thread_id ${HDF5_TEST_SOURCE_DIR}/thread_id.c)
target_compile_options(thread_id PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(thread_id PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (thread_id PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (thread_id STATIC)
  target_link_libraries (thread_id PRIVATE ${HDF5_TEST_LIB_TARGET})
  if (NOT WIN32)
    target_link_libraries (thread_id PRIVATE "$<$<BOOL:${HDF5_ENABLE_THREADSAFE}>:Threads::Threads>")
  endif ()
else ()
  TARGET_C_PROPERTIES (thread_id SHARED)
  target_link_libraries (thread_id PRIVATE ${HDF5_TEST_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_THREADSAFE}>:Threads::Threads>")
endif ()
set_target_properties (thread_id PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_thread_id_FORMAT thread_id)
endif ()

if (HDF5_BUILD_UTILS) # requires mirror server
  #-- Adding test for mirror_vfd
  add_executable (mirror_vfd ${mirror_vfd_SOURCES})
  target_include_directories (mirror_vfd PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (mirror_vfd STATIC)
    target_link_libraries (mirror_vfd PRIVATE ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (mirror_vfd SHARED)
    target_link_libraries (mirror_vfd PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (mirror_vfd PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_mirror_vfd_FORMAT mirror_vfd)
  endif ()
endif ()

##############################################################################
###           A D D I T I O N A L   T E S T S                              ###
##############################################################################

set (H5_CHECK_TESTS
    error_test
    err_compat
    tcheck_version
    testmeta
    atomic_writer
    atomic_reader
    links_env
    filenotclosed
    del_many_dense_attrs
    flushrefresh
)

foreach (h5_test ${H5_CHECK_TESTS})
  ADD_H5_EXE(${h5_test})
endforeach ()

#-- Adding test for libinfo
set (GREP_RUNNER ${PROJECT_BINARY_DIR}/GrepRunner.cmake)
file (WRITE ${GREP_RUNNER} "file (STRINGS \${TEST_PROGRAM} TEST_RESULT REGEX \"SUMMARY OF THE HDF5 CONFIGURATION\")
if (NOT TEST_RESULT)
  message (FATAL_ERROR \"Failed: The output: \${TEST_RESULT} of \${TEST_PROGRAM} did not contain SUMMARY OF THE HDF5 CONFIGURATION\")
else ()
  message (STATUS \"COMMAND Result: \${TEST_RESULT}\")
endif ()
"
)

##############################################################################
###                S W I M M E R   T E S T S                              ###
##############################################################################

set (H5_SWMR_TESTS
    swmr_addrem_writer
    swmr_generator
    swmr_reader
    swmr_remove_reader
    swmr_remove_writer
    swmr_sparse_reader
    swmr_sparse_writer
    swmr_start_write
    swmr_writer
)

foreach (h5_test ${H5_SWMR_TESTS})
  ADD_H5_EXE(${h5_test})
endforeach ()

set (H5_VDS_SWMR_TESTS
    vds_swmr_gen
    vds_swmr_reader
    vds_swmr_writer
)

macro (ADD_H5_VDS_EXE file)
  add_executable (${file} ${HDF5_TEST_SOURCE_DIR}/${file}.c ${HDF5_TEST_SOURCE_DIR}/vds_swmr.h)
  target_include_directories (${file} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_BINARY_DIR};${HDF5_TEST_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(${file} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${file} PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
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

foreach (h5_test ${H5_VDS_SWMR_TESTS})
  ADD_H5_VDS_EXE(${h5_test})
endforeach ()

#-- Adding test for accum_swmr_reader
#   This has to be copied to the test directory for execve() to find it
#   and it can't be renamed (i.e., no <foo>-shared).
add_executable (accum_swmr_reader ${HDF5_TEST_SOURCE_DIR}/accum_swmr_reader.c)
target_compile_options(accum_swmr_reader PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(accum_swmr_reader PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (accum_swmr_reader PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (accum_swmr_reader STATIC)
  target_link_libraries (accum_swmr_reader PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (accum_swmr_reader SHARED)
  target_link_libraries (accum_swmr_reader PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (accum_swmr_reader PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_accum_swmr_reader_FORMAT accum_swmr_reader)
endif ()

#-- Set accum dependencies
set_target_properties (accum PROPERTIES DEPENDS accum_swmr_reader)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  add_executable (filter_plugin ${HDF5_TEST_SOURCE_DIR}/filter_plugin.c)
  target_include_directories (filter_plugin PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (filter_plugin SHARED)
  target_link_libraries (filter_plugin PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  set_target_properties (filter_plugin PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_filter_plugin_FORMAT filter_plugin)
  endif ()

  add_executable (vfd_plugin ${HDF5_TEST_SOURCE_DIR}/vfd_plugin.c)
  target_include_directories (vfd_plugin PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (vfd_plugin SHARED)
  target_link_libraries (vfd_plugin PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  set_target_properties (vfd_plugin PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_vfd_plugin_FORMAT vfd_plugin)
  endif ()

  add_executable (vol_plugin ${HDF5_TEST_SOURCE_DIR}/vol_plugin.c)
  target_include_directories (vol_plugin PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (vol_plugin SHARED)
  target_link_libraries (vol_plugin PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  set_target_properties (vol_plugin PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_vol_plugin_FORMAT vol_plugin)
  endif ()
endif ()

##############################################################################
###    U S E  C A S E S  T E S T S
##############################################################################
set (use_append_chunk_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_append_chunk.c ${HDF5_TEST_SOURCE_DIR}/use_common.c ${HDF5_TEST_SOURCE_DIR}/use.h)
add_executable (use_append_chunk ${use_append_chunk_SOURCES})
target_compile_options(use_append_chunk PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(use_append_chunk PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (use_append_chunk PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (use_append_chunk STATIC)
  target_link_libraries (use_append_chunk PRIVATE ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (use_append_chunk SHARED)
  target_link_libraries (use_append_chunk PRIVATE ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (use_append_chunk PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_use_append_chunk_FORMAT use_append_chunk)
endif ()

if (HDF5_BUILD_UTILS) # requires mirror server
  set (use_append_chunk_mirror_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_append_chunk_mirror.c ${HDF5_TEST_SOURCE_DIR}/use_common.c ${HDF5_TEST_SOURCE_DIR}/use.h)
  add_executable (use_append_chunk_mirror ${use_append_chunk_mirror_SOURCES})
  target_compile_options(use_append_chunk_mirror PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(use_append_chunk_mirror PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
  target_include_directories (use_append_chunk_mirror PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (use_append_chunk_mirror STATIC)
    target_link_libraries (use_append_chunk_mirror PRIVATE ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (use_append_chunk_mirror SHARED)
    target_link_libraries (use_append_chunk_mirror PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (use_append_chunk_mirror PROPERTIES FOLDER test)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_use_append_chunk_mirror_FORMAT use_append_chunk_mirror)
  endif ()
endif ()

set (use_append_mchunks_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_append_mchunks.c ${HDF5_TEST_SOURCE_DIR}/use_common.c ${HDF5_TEST_SOURCE_DIR}/use.h)
add_executable (use_append_mchunks ${use_append_mchunks_SOURCES})
target_compile_options(use_append_mchunks PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(use_append_mchunks PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (use_append_mchunks PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (use_append_mchunks STATIC)
  target_link_libraries (use_append_mchunks PRIVATE ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (use_append_mchunks SHARED)
  target_link_libraries (use_append_mchunks PRIVATE ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (use_append_mchunks PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_use_append_mchunks_FORMAT use_append_mchunks)
endif ()

set (use_disable_mdc_flushes_SOURCES ${HDF5_TEST_SOURCE_DIR}/use_disable_mdc_flushes.c)
add_executable (use_disable_mdc_flushes ${use_disable_mdc_flushes_SOURCES})
target_compile_options(use_disable_mdc_flushes PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(use_disable_mdc_flushes PRIVATE "${HDF5_TEST_COMPILE_DEFS_PRIVATE}")
target_include_directories (use_disable_mdc_flushes PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (use_disable_mdc_flushes STATIC)
  target_link_libraries (use_disable_mdc_flushes PRIVATE ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (use_disable_mdc_flushes SHARED)
  target_link_libraries (use_disable_mdc_flushes PRIVATE ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (use_disable_mdc_flushes PROPERTIES FOLDER test)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_use_disable_mdc_flushes_FORMAT use_disable_mdc_flushes)
endif ()

if (HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()

if (HDF5_TEST_API)
  add_subdirectory (API)
endif ()
