cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TOOLS_H5DIFF)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5diff executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5diff
    ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_common.c
    ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_main.c
)
TARGET_NAMING (h5diff ${LIB_TYPE})
TARGET_C_PROPERTIES (h5diff " " " ")
TARGET_LINK_LIBRARIES (h5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5diff PROPERTIES FOLDER tools)

SET (H5_DEP_EXECUTABLES h5diff)

IF (H5_HAVE_PARALLEL)
  ADD_EXECUTABLE (ph5diff
      ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/ph5diff_main.c
  )
  TARGET_NAMING (ph5diff ${LIB_TYPE})
  TARGET_C_PROPERTIES (ph5diff " " " ")
  TARGET_LINK_LIBRARIES (ph5diff  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (ph5diff PROPERTIES FOLDER tools)
ENDIF (H5_HAVE_PARALLEL)

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5diff and test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5diffgentest ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/h5diffgentest.c)
    TARGET_NAMING (h5diffgentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5diffgentest " " " ")
    TARGET_LINK_LIBRARIES (h5diffgentest ${HDF5_LIB_TARGET})
    SET_TARGET_PROPERTIES (h5diffgentest PROPERTIES FOLDER generator/tools)
    
    #ADD_TEST (NAME h5diffgentest COMMAND $<TARGET_FILE:h5diffgentest>)
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

#INSTALL_PROGRAM_PDB (h5diff ${HDF5_INSTALL_BIN_DIR} toolsapplications)

INSTALL (
    TARGETS
        h5diff
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)

IF (H5_HAVE_PARALLEL)

  #INSTALL_PROGRAM_PDB (ph5diff ${HDF5_INSTALL_BIN_DIR} toolsapplications)

  INSTALL (
      TARGETS
          ph5diff
      RUNTIME DESTINATION
          ${HDF5_INSTALL_BIN_DIR}
      COMPONENT
          toolsapplications
  )
ENDIF (H5_HAVE_PARALLEL)
