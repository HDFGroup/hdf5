cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_H5LS)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

#-----------------------------------------------------------------------------
# Add the h5ls executable
#-----------------------------------------------------------------------------
add_executable (h5ls ${HDF5_TOOLS_H5LS_SOURCE_DIR}/h5ls.c)
TARGET_NAMING (h5ls STATIC)
TARGET_C_PROPERTIES (h5ls STATIC " " " ")
target_link_libraries (h5ls  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5ls PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5ls")

set (H5_DEP_EXECUTABLES
    h5ls
)

if (BUILD_TESTING)

  #-----------------------------------------------------------------------------
  # If plugin library tests can be tested
  #-----------------------------------------------------------------------------
  set (HDF5_TOOL_PLUGIN_LIB_CORENAME         "dynlibls")
  set (HDF5_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_CORENAME}")
  set (HDF5_TOOL_PLUGIN_LIB_TARGET           ${HDF5_TOOL_PLUGIN_LIB_CORENAME})
  add_definitions (${HDF_EXTRA_C_FLAGS})
  INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

  add_library (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_ls.c)
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

  include (CMakeTests.cmake)

endif (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5ls ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5ls
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
