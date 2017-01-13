cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5COPY)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5copy and test executables
# --------------------------------------------------------------------
add_executable (h5copy ${HDF5_TOOLS_SRC_H5COPY_SOURCE_DIR}/h5copy.c)
TARGET_NAMING (h5copy STATIC)
TARGET_C_PROPERTIES (h5copy STATIC " " " ")
target_link_libraries (h5copy  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5copy PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5copy")

set (H5_DEP_EXECUTABLES h5copy)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5copy ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5copy
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
