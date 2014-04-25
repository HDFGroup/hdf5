cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
add_executable (h5stat ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat.c)
TARGET_NAMING (h5stat ${LIB_TYPE})
TARGET_C_PROPERTIES (h5stat " " " ")
target_link_libraries (h5stat  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5stat PROPERTIES FOLDER tools)

set (H5_DEP_EXECUTABLES h5stat)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5stat test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    add_executable (h5stat_gentest ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat_gentest.c)
    TARGET_NAMING (h5stat_gentest ${LIB_TYPE})
    TARGET_C_PROPERTIES (h5stat_gentest " " " ")
    target_link_libraries (h5stat_gentest ${HDF5_LIB_TARGET})
    set_target_properties (h5stat_gentest PROPERTIES FOLDER generator/tools)
    
    #add_test (NAME h5stat_gentest COMMAND $<TARGET_FILE:h5stat_gentest>)
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

#INSTALL_PROGRAM_PDB (h5stat ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5stat
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}
    COMPONENT
        toolsapplications
)
