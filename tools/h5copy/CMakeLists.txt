cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TOOLS_H5COPY)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5copy and test executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5copy ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copy.c)
TARGET_NAMING (h5copy ${LIB_TYPE})
TARGET_C_PROPERTIES (h5copy " " " ")
TARGET_LINK_LIBRARIES (h5copy  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5copy PROPERTIES FOLDER tools)

SET (H5_DEP_EXECUTABLES h5copy)

IF (BUILD_TESTING)
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5copygentest ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copygentest.c)
    TARGET_NAMING (h5copygentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5copygentest " " " ")
    TARGET_LINK_LIBRARIES (h5copygentest ${HDF5_LIB_TARGET})
    SET_TARGET_PROPERTIES (h5copygentest PROPERTIES FOLDER generator/tools)

    #ADD_TEST (NAME h5copygentest COMMAND $<TARGET_FILE:h5copygentest>)
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

#INSTALL_PROGRAM_PDB (h5copy ${HDF5_INSTALL_BIN_DIR} toolsapplications)
  
INSTALL (
    TARGETS
        h5copy
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
