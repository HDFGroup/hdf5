cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5DUMP)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5dump executables
# --------------------------------------------------------------------
add_executable (h5dump
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump.c
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_ddl.c
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_xml.c
)
TARGET_NAMING (h5dump STATIC)
TARGET_C_PROPERTIES (h5dump STATIC " " " ")
target_link_libraries (h5dump  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5dump PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5dump")

set (H5_DEP_EXECUTABLES h5dump)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5dump ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5dump
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
