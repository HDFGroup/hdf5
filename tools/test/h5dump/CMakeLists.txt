cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_H5DUMP)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  #-----------------------------------------------------------------------------
  # If plugin library tests can be tested
  #-----------------------------------------------------------------------------
  set (HDF5_TOOL_PLUGIN_LIB_CORENAME         "dynlibdump")
  set (HDF5_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_CORENAME}")
  set (HDF5_TOOL_PLUGIN_LIB_TARGET           ${HDF5_TOOL_PLUGIN_LIB_CORENAME})
  add_definitions (${HDF_EXTRA_C_FLAGS})
  INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

  add_library (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_dump.c)
  TARGET_C_PROPERTIES (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED " " " ")
  target_link_libraries (${HDF5_TOOL_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  H5_SET_LIB_OPTIONS (${HDF5_TOOL_PLUGIN_LIB_TARGET} ${HDF5_TOOL_PLUGIN_LIB_NAME} SHARED ${HDF5_PACKAGE_SOVERSION})

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

  # --------------------------------------------------------------------
  # Add the h5dump test executable
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5dumpgentest ${HDF5_TOOLS_TEST_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
    TARGET_NAMING (h5dumpgentest STATIC)
    TARGET_C_PROPERTIES (h5dumpgentest STATIC " " " ")
    target_link_libraries (h5dumpgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5dumpgentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5dumpgentest COMMAND $<TARGET_FILE:h5dumpgentest>)
  endif ()

  include (CMakeTests.cmake)

  include (CMakeTestsPBITS.cmake)

  include (CMakeTestsVDS.cmake)

  include (CMakeTestsXML.cmake)
