cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS_H5REPACK)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})

# --------------------------------------------------------------------
# Add h5Repack executables
# --------------------------------------------------------------------
set (REPACK_COMMON_SRCS
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_copy.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_filters.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_opttable.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_parse.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_refs.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_verify.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack.c
)

add_executable (h5repack ${REPACK_COMMON_SRCS} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_main.c)
TARGET_NAMING (h5repack ${LIB_TYPE})
TARGET_C_PROPERTIES (h5repack " " " ")
target_link_libraries (h5repack  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5repack PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES h5repack)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add h5Repack test executables
  # --------------------------------------------------------------------
  add_executable (testh5repack_detect_szip ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testh5repack_detect_szip.c)
  TARGET_NAMING (testh5repack_detect_szip ${LIB_TYPE})
  TARGET_C_PROPERTIES (testh5repack_detect_szip " " " ")
  target_link_libraries (testh5repack_detect_szip ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (testh5repack_detect_szip PROPERTIES FOLDER tools)

  add_executable (h5repacktest ${REPACK_COMMON_SRCS} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repacktst.c)
  TARGET_NAMING (h5repacktest ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5repacktest " " " ")
  target_link_libraries (h5repacktest  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (h5repacktest PROPERTIES FOLDER tools)

  #-----------------------------------------------------------------------------
  # If plugin library tests can be tested
  #-----------------------------------------------------------------------------
  if (BUILD_SHARED_LIBS)
    set (HDF5_TOOL_PLUGIN_LIB_CORENAME         "dynlibadd")
    set (HDF5_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_CORENAME}")
    set (HDF5_TOOL_PLUGIN_LIB_TARGET           ${HDF5_TOOL_PLUGIN_LIB_CORENAME})
    add_definitions (${HDF_EXTRA_C_FLAGS})
    INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

    add_library (${HDF5_TOOL_PLUGIN_LIB_TARGET} ${LIB_TYPE} dynlib_rpk.c)
    TARGET_C_PROPERTIES (${HDF5_TOOL_PLUGIN_LIB_TARGET} " " " ")
    target_link_libraries (${HDF5_TOOL_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (
        ${HDF5_TOOL_PLUGIN_LIB_TARGET} ${HDF5_TOOL_PLUGIN_LIB_NAME}
        ${LIB_TYPE}
        HDF5_TOOL_PLUGIN_LIB_NAME_RELEASE
        HDF5_TOOL_PLUGIN_LIB_NAME_DEBUG
    )

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
  endif (BUILD_SHARED_LIBS)

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

#INSTALL_PROGRAM_PDB (h5repack ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5repack
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
