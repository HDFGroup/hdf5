# CMake configuration for building and testing the h5dump tool
# Configures dynamic plugin library for h5dump if shared builds are enabled
# Sets up include directories, linking, and formatting for plugin targets
# Includes test scripts for h5dump, PBITS, VDS, and XML if test tools and serial tests are enabled

cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_TEST_H5DUMP C)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  set (H5DUMP_TOOL_PLUGIN_LIB_CORENAME         "dynlibdump")
  set (H5DUMP_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5DUMP_TOOL_PLUGIN_LIB_CORENAME}")
  set (H5DUMP_TOOL_PLUGIN_LIB_TARGET           ${H5DUMP_TOOL_PLUGIN_LIB_CORENAME})

  add_library (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_dump.c)
  target_include_directories (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} SHARED)
  target_link_libraries (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_LIBSH_TARGET})
  H5_SET_LIB_OPTIONS (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} ${H5DUMP_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_H5DUMP_PLUGIN_FORMAT ${H5DUMP_TOOL_PLUGIN_LIB_TARGET})
  endif ()

  # make plugins dir
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
  #-----------------------------------------------------------------------------
  # Copy plugin library to a plugins folder
  #-----------------------------------------------------------------------------
  add_custom_command (
      TARGET     ${H5DUMP_TOOL_PLUGIN_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different
            "$<TARGET_FILE:${H5DUMP_TOOL_PLUGIN_LIB_TARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5DUMP_TOOL_PLUGIN_LIB_TARGET}>"
  )
endif ()

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)

  include (CMakeTestsPBITS.cmake)

  include (CMakeTestsVDS.cmake)

  include (CMakeTestsXML.cmake)
endif ()
