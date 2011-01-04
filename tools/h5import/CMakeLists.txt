cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5import ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5import.c)
H5_NAMING (h5import ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5import  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5import)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5import executables
  # --------------------------------------------------------------------
  ADD_EXECUTABLE (h5importtest ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5importtest.c)
  H5_NAMING (h5importtest ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5IMPORT-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          binfp64.bin
          binin8.bin
          binin8w.bin
          binin16.bin
          binin32.bin
          binuin16.bin
          binuin32.bin
          txtin16.txt
          txtin32.txt
  )

  ADD_TEST (NAME h5importtest COMMAND $<TARGET_FILE:h5importtest>)
ENDIF (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5import
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
