cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
add_executable (h5import ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5import.c)
TARGET_NAMING (h5import ${LIB_TYPE})
TARGET_C_PROPERTIES (h5import " " " ")
target_link_libraries (h5import  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
#set_target_properties (h5import PROPERTIES COMPILE_DEFINITIONS H5DEBUGIMPORT)
set_target_properties (h5import PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES h5import)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5import executables
  # --------------------------------------------------------------------
  add_executable (h5importtest ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5importtest.c)
  TARGET_NAMING (h5importtest ${LIB_TYPE})
  TARGET_C_PROPERTIES (h5importtest " " " ")
  target_link_libraries (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5importtest PROPERTIES FOLDER tools)

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

#INSTALL_PROGRAM_PDB (h5import ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5import
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
