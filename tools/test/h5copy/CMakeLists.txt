#-----------------------------------------------------------------------------
# CMake configuration for HDF5 h5copy tool plugin tests
# This file sets up the build for the dynamic library plugin used in h5copy tests.
# It handles plugin compilation, formatting, and installation into the plugins directory.
# It also includes test configuration if enabled.
#-----------------------------------------------------------------------------
cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_TEST_H5COPY C)

#-----------------------------------------------------------------------------
# If plugin library tests can be tested
#-----------------------------------------------------------------------------
if (BUILD_SHARED_LIBS)
  # Set plugin library core name, target, and output name
  set (H5COPY_TOOL_PLUGIN_LIB_CORENAME         "dynlibcopy")
  set (H5COPY_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5COPY_TOOL_PLUGIN_LIB_CORENAME}")
  set (H5COPY_TOOL_PLUGIN_LIB_TARGET           ${H5COPY_TOOL_PLUGIN_LIB_CORENAME})

  # Add the dynamic library for the h5copy tool plugin
  add_library (${H5COPY_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_copy.c)
  # Set include directories for the plugin build
  target_include_directories (${H5COPY_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  # Set C properties and link libraries
  TARGET_C_PROPERTIES (${H5COPY_TOOL_PLUGIN_LIB_TARGET} SHARED)
  target_link_libraries (${H5COPY_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_LIBSH_TARGET})
  # Set library options for the plugin
  H5_SET_LIB_OPTIONS (${H5COPY_TOOL_PLUGIN_LIB_TARGET} ${H5COPY_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_H5COPY_PLUGIN_FORMAT ${H5COPY_TOOL_PLUGIN_LIB_TARGET})
  endif ()

  # Make plugins directory for output
  file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")

  #-----------------------------------------------------------------------------
  # Copy plugin library to a plugins folder
  #-----------------------------------------------------------------------------
  add_custom_command (
      TARGET     ${H5COPY_TOOL_PLUGIN_LIB_TARGET}
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different
          "$<TARGET_FILE:${H5COPY_TOOL_PLUGIN_LIB_TARGET}>"
          "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5COPY_TOOL_PLUGIN_LIB_TARGET}>"
  )
endif ()

# If tool and serial tests are enabled, include test configuration
if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
