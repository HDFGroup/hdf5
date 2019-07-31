cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5DIFF C)

# --------------------------------------------------------------------
# Add the h5diff and test executables
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
  add_executable (h5diffgentest ${HDF5_TOOLS_TEST_H5DIFF_SOURCE_DIR}/h5diffgentest.c)
  target_include_directories (h5diffgentest PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5diffgentest STATIC)
  target_link_libraries (h5diffgentest PRIVATE ${HDF5_LIB_TARGET})
  set_target_properties (h5diffgentest PROPERTIES FOLDER generator/tools)

  #add_test (NAME h5diffgentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diffgentest>)
endif ()

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  set (H5DIFF_TOOL_PLUGIN_LIB_CORENAME         "dynlibdiff")
  set (H5DIFF_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5DIFF_TOOL_PLUGIN_LIB_CORENAME}")
  set (H5DIFF_TOOL_PLUGIN_LIB_TARGET           ${H5DIFF_TOOL_PLUGIN_LIB_CORENAME})

  add_library (${H5DIFF_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_diff.c)
  target_include_directories (${H5DIFF_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (${H5DIFF_TOOL_PLUGIN_LIB_TARGET} SHARED)
  target_link_libraries (${H5DIFF_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  H5_SET_LIB_OPTIONS (${H5DIFF_TOOL_PLUGIN_LIB_TARGET} ${H5DIFF_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
  #-----------------------------------------------------------------------------
  # Copy plugin library to a plugins folder
  #-----------------------------------------------------------------------------
  add_custom_command (
      TARGET     ${H5DIFF_TOOL_PLUGIN_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different
          "$<TARGET_FILE:${H5DIFF_TOOL_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5DIFF_TOOL_PLUGIN_LIB_TARGET}>"
  )
endif ()

include (CMakeTests.cmake)
