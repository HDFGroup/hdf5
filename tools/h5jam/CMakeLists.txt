cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_H5JAM)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5jam executables
# --------------------------------------------------------------------
add_executable (h5jam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jam.c)
TARGET_NAMING (h5jam STATIC)
TARGET_C_PROPERTIES (h5jam STATIC " " " ")
target_link_libraries (h5jam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5jam PROPERTIES FOLDER tools)

add_executable (getub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/getub.c)
TARGET_NAMING (getub STATIC)
TARGET_C_PROPERTIES (getub STATIC " " " ")
target_link_libraries (getub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (getub PROPERTIES FOLDER tools)

add_executable (tellub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/tellub.c)
TARGET_NAMING (tellub STATIC)
TARGET_C_PROPERTIES (tellub STATIC " " " ")
target_link_libraries (tellub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (tellub PROPERTIES FOLDER tools)

add_executable (h5unjam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5unjam.c)
TARGET_NAMING (h5unjam STATIC)
TARGET_C_PROPERTIES (h5unjam STATIC " " " ")
target_link_libraries (h5unjam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5unjam PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES
    h5jam
    getub
    tellub
    h5unjam
)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5jam test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5jamgentest ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jamgentest.c)
    TARGET_NAMING (h5jamgentest STATIC)
    TARGET_C_PROPERTIES (testhdf5 STATIC " " " ")
    target_link_libraries (h5jamgentest ${HDF5_LIB_TARGET})
    set_target_properties (h5jamgentest PROPERTIES FOLDER generator/tools)
    
    #add_test (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)
  endif (HDF5_BUILD_GENERATORS)

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

#INSTALL_PROGRAM_PDB (h5jam ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5jam h5unjam
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
