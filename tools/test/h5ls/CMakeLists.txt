cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5LS C)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  set (H5LS_TOOL_PLUGIN_LIB_CORENAME         "dynlibls")
  set (H5LS_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5LS_TOOL_PLUGIN_LIB_CORENAME}")
  set (H5LS_TOOL_PLUGIN_LIB_TARGET           ${H5LS_TOOL_PLUGIN_LIB_CORENAME})

  add_library (${H5LS_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_ls.c)
  target_include_directories (${H5LS_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (${H5LS_TOOL_PLUGIN_LIB_TARGET} SHARED)
  target_link_libraries (${H5LS_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  H5_SET_LIB_OPTIONS (${H5LS_TOOL_PLUGIN_LIB_TARGET} ${H5LS_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
  #-----------------------------------------------------------------------------
  # Copy plugin library to a plugins folder
  #-----------------------------------------------------------------------------
  add_custom_command (
      TARGET     ${H5LS_TOOL_PLUGIN_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different
          "$<TARGET_FILE:${H5LS_TOOL_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5LS_TOOL_PLUGIN_LIB_TARGET}>"
  )
endif ()

include (CMakeTests.cmake)

include (CMakeTestsVDS.cmake)
