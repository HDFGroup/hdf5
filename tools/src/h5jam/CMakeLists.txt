cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5JAM)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5jam executables
# --------------------------------------------------------------------
add_executable (h5jam ${HDF5_TOOLS_SRC_H5JAM_SOURCE_DIR}/h5jam.c)
TARGET_NAMING (h5jam STATIC)
TARGET_C_PROPERTIES (h5jam STATIC " " " ")
target_link_libraries (h5jam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5jam PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5jam")

add_executable (h5unjam ${HDF5_TOOLS_SRC_H5JAM_SOURCE_DIR}/h5unjam.c)
TARGET_NAMING (h5unjam STATIC)
TARGET_C_PROPERTIES (h5unjam STATIC " " " ")
target_link_libraries (h5unjam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5unjam PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5unjam")

set (H5_DEP_EXECUTABLES
    h5jam
    h5unjam
)

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
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
