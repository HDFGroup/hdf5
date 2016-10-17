cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_H5LS)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

#-----------------------------------------------------------------------------
# Add the h5ls executable
#-----------------------------------------------------------------------------
add_executable (h5ls ${HDF5_TOOLS_H5LS_SOURCE_DIR}/h5ls.c)
TARGET_NAMING (h5ls STATIC)
TARGET_C_PROPERTIES (h5ls STATIC " " " ")
target_link_libraries (h5ls  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5ls PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5ls")

set (H5_DEP_EXECUTABLES
    h5ls
)

if (BUILD_TESTING)

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

#INSTALL_PROGRAM_PDB (h5ls ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5ls
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
