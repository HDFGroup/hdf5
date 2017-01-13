cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
add_executable (h5import ${HDF5_TOOLS_SRC_H5IMPORT_SOURCE_DIR}/h5import.c)
TARGET_NAMING (h5import STATIC)
TARGET_C_PROPERTIES (h5import STATIC " " " ")
target_link_libraries (h5import  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
#set_target_properties (h5import PROPERTIES COMPILE_DEFINITIONS H5DEBUGIMPORT)
set_target_properties (h5import PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5import")

set (H5_DEP_EXECUTABLES h5import)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5import ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5import
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
