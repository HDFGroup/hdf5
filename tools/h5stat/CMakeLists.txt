cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TOOLS_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5stat ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat.c)
TARGET_NAMING (h5stat ${LIB_TYPE})
TARGET_C_PROPERTIES (h5stat " " " ")
TARGET_LINK_LIBRARIES (h5stat  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5stat PROPERTIES FOLDER tools)

SET (H5_DEP_EXECUTABLES h5stat)

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5stat test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5stat_gentest ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat_gentest.c)
    TARGET_NAMING (h5stat_gentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5stat_gentest " " " ")
    TARGET_LINK_LIBRARIES (h5stat_gentest ${HDF5_LIB_TARGET})
    SET_TARGET_PROPERTIES (h5stat_gentest PROPERTIES FOLDER generator/tools)
    
    #ADD_TEST (NAME h5stat_gentest COMMAND $<TARGET_FILE:h5stat_gentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

  INCLUDE (CMakeTests.cmake)
ENDIF (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5stat ${HDF5_INSTALL_BIN_DIR} toolsapplications)

INSTALL (
    TARGETS
        h5stat
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
