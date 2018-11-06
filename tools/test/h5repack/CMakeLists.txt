cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5REPACK C)

# --------------------------------------------------------------------
# Add h5Repack test executables
# --------------------------------------------------------------------
add_executable (testh5repack_detect_szip ${HDF5_TOOLS_TEST_H5REPACK_SOURCE_DIR}/testh5repack_detect_szip.c)
target_include_directories(testh5repack_detect_szip
    PRIVATE "${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
)
TARGET_C_PROPERTIES (testh5repack_detect_szip STATIC)
target_link_libraries (testh5repack_detect_szip PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (testh5repack_detect_szip PROPERTIES FOLDER tools)

set (REPACK_COMMON_SOURCES
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_copy.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_filters.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_opttable.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_parse.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_refs.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_verify.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack.c
)
add_executable (h5repacktest ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_TEST_H5REPACK_SOURCE_DIR}/h5repacktst.c)
target_include_directories(h5repacktest
    PRIVATE "${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
)
TARGET_C_PROPERTIES (h5repacktest STATIC)
target_link_libraries (h5repacktest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
set_target_properties (h5repacktest PROPERTIES FOLDER tools)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  set (HDF5_TOOL_PLUGIN_LIB_CORENAME         "dynlibadd")
  set (HDF5_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_CORENAME}")
  set (HDF5_TOOL_PLUGIN_LIB_TARGET           ${HDF5_TOOL_PLUGIN_LIB_CORENAME})
  set (HDF5_TOOL_PLUGIN_LIB_VCORENAME        "dynlibvers")
  set (HDF5_TOOL_PLUGIN_LIB_VNAME            "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_VCORENAME}")
  set (HDF5_TOOL_PLUGIN_LIB_VTARGET          ${HDF5_TOOL_PLUGIN_LIB_VCORENAME})

  add_library (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_rpk.c)
  target_include_directories(${HDF5_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED)
  target_link_libraries (${HDF5_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_TEST_LIB_TARGET})
  H5_SET_LIB_OPTIONS (${HDF5_TOOL_PLUGIN_LIB_TARGET} ${HDF5_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

  add_library (${HDF5_TOOL_PLUGIN_LIB_VTARGET} SHARED dynlib_vrpk.c)
  target_include_directories(${HDF5_TOOL_PLUGIN_LIB_VTARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (${HDF5_TOOL_PLUGIN_LIB_VTARGET} SHARED)
  target_link_libraries (${HDF5_TOOL_PLUGIN_LIB_VTARGET} PRIVATE ${HDF5_TEST_LIB_TARGET})
  H5_SET_LIB_OPTIONS (${HDF5_TOOL_PLUGIN_LIB_VTARGET} ${HDF5_TOOL_PLUGIN_LIB_VNAME} SHARED "LIB")

  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
  #-----------------------------------------------------------------------------
  # Copy plugin library to a plugins folder
  #-----------------------------------------------------------------------------
  add_custom_command (
      TARGET     ${HDF5_TOOL_PLUGIN_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different
            "$<TARGET_FILE:${HDF5_TOOL_PLUGIN_LIB_TARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${HDF5_TOOL_PLUGIN_LIB_TARGET}>"
  )
  add_custom_command (
      TARGET     ${HDF5_TOOL_PLUGIN_LIB_VTARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different
            "$<TARGET_FILE:${HDF5_TOOL_PLUGIN_LIB_VTARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${HDF5_TOOL_PLUGIN_LIB_VTARGET}>"
  )
endif ()

include (CMakeTests.cmake)
