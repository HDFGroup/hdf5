cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5FC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5format_convert executables
# --------------------------------------------------------------------
add_executable (h5format_convert ${HDF5_TOOLS_SRC_H5FC_SOURCE_DIR}/h5format_convert.c)
TARGET_NAMING (h5format_convert STATIC)
TARGET_C_PROPERTIES (h5format_convert STATIC " " " ")
target_link_libraries (h5format_convert  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5format_convert PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5format_convert")

set (H5_DEP_EXECUTABLES h5format_convert)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5format_convert ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5format_convert
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
