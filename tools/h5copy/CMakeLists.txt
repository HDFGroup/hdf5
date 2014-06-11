cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS_H5COPY)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5copy and test executables
# --------------------------------------------------------------------
add_executable (h5copy ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copy.c)
TARGET_NAMING (h5copy ${LIB_TYPE})
TARGET_C_PROPERTIES (h5copy " " " ")
target_link_libraries (h5copy  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5copy PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES h5copy)

if (BUILD_TESTING)
  if (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    add_executable (h5copygentest ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copygentest.c)
    TARGET_NAMING (h5copygentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5copygentest " " " ")
    target_link_libraries (h5copygentest ${HDF5_LIB_TARGET})
    set_target_properties (h5copygentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5copygentest COMMAND $<TARGET_FILE:h5copygentest>)
  endif (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

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

#INSTALL_PROGRAM_PDB (h5copy ${HDF5_INSTALL_BIN_DIR} toolsapplications)
  
install (
    TARGETS
        h5copy
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
