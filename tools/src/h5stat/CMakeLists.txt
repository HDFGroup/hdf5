cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
add_executable (h5stat ${HDF5_TOOLS_SRC_H5STAT_SOURCE_DIR}/h5stat.c)
TARGET_NAMING (h5stat STATIC)
TARGET_C_PROPERTIES (h5stat STATIC " " " ")
target_link_libraries (h5stat  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5stat PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5stat")

set (H5_DEP_EXECUTABLES h5stat)

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
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
