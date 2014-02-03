cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TOOLS_H5JAM)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5jam executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5jam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jam.c)
TARGET_NAMING (h5jam ${LIB_TYPE})
TARGET_C_PROPERTIES (h5jam " " " ")
TARGET_LINK_LIBRARIES (h5jam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5jam PROPERTIES FOLDER tools)

ADD_EXECUTABLE (getub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/getub.c)
TARGET_NAMING (getub ${LIB_TYPE})
TARGET_C_PROPERTIES (getub " " " ")
TARGET_LINK_LIBRARIES (getub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (getub PROPERTIES FOLDER tools)

ADD_EXECUTABLE (tellub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/tellub.c)
TARGET_NAMING (tellub ${LIB_TYPE})
TARGET_C_PROPERTIES (tellub " " " ")
TARGET_LINK_LIBRARIES (tellub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (tellub PROPERTIES FOLDER tools)

ADD_EXECUTABLE (h5unjam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5unjam.c)
TARGET_NAMING (h5unjam ${LIB_TYPE})
TARGET_C_PROPERTIES (h5unjam " " " ")
TARGET_LINK_LIBRARIES (h5unjam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5unjam PROPERTIES FOLDER tools)

SET (H5_DEP_EXECUTABLES
    h5jam
    getub
    tellub
    h5unjam
)

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5jam test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5jamgentest ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jamgentest.c)
    TARGET_NAMING (h5jamgentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (testhdf5 " " " ")
    TARGET_LINK_LIBRARIES (h5jamgentest ${HDF5_LIB_TARGET})
    SET_TARGET_PROPERTIES (h5jamgentest PROPERTIES FOLDER generator/tools)
    
    #ADD_TEST (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)
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

#INSTALL_PROGRAM_PDB (h5jam ${HDF5_INSTALL_BIN_DIR} toolsapplications)

INSTALL (
    TARGETS
        h5jam h5unjam
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
