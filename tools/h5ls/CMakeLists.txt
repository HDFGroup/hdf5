cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TOOLS_H5LS)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

#-----------------------------------------------------------------------------
# Add the h5ls executable
#-----------------------------------------------------------------------------
ADD_EXECUTABLE (h5ls ${HDF5_TOOLS_H5LS_SOURCE_DIR}/h5ls.c)
TARGET_NAMING (h5ls ${LIB_TYPE})
TARGET_C_PROPERTIES (h5ls " " " ")
TARGET_LINK_LIBRARIES (h5ls  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5ls PROPERTIES FOLDER tools)

SET (H5_DEP_EXECUTABLES
    h5ls
)

IF (BUILD_TESTING)

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

#INSTALL_PROGRAM_PDB (h5ls ${HDF5_INSTALL_BIN_DIR} toolsapplications)

INSTALL (
    TARGETS
        h5ls
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
