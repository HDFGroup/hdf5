cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5DUMP C)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  set (H5DUMP_TOOL_PLUGIN_LIB_CORENAME         "dynlibdump")
  set (H5DUMP_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5DUMP_TOOL_PLUGIN_LIB_CORENAME}")
  set (H5DUMP_TOOL_PLUGIN_LIB_TARGET           ${H5DUMP_TOOL_PLUGIN_LIB_CORENAME})

  add_library (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_dump.c)
  target_include_directories (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} SHARED)
  target_link_libraries (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  H5_SET_LIB_OPTIONS (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} ${H5DUMP_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

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

# --------------------------------------------------------------------
# Add the h5dump test executable
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
  add_executable (h5dumpgentest ${HDF5_TOOLS_TEST_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
  target_include_directories (h5dumpgentest PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5dumpgentest STATIC)
  target_link_libraries (h5dumpgentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5dumpgentest PROPERTIES FOLDER generator/tools)

  #add_test (NAME h5dumpgentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dumpgentest>)
endif ()

include (CMakeTests.cmake)

include (CMakeTestsPBITS.cmake)

include (CMakeTestsVDS.cmake)

include (CMakeTestsXML.cmake)
