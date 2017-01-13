cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5REPACK)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})

# --------------------------------------------------------------------
# Add h5Repack executables
# --------------------------------------------------------------------
set (REPACK_COMMON_SOURCES
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_copy.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_filters.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_opttable.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_parse.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_refs.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_verify.c
    ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack.c
)

add_executable (h5repack ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR}/h5repack_main.c)
TARGET_NAMING (h5repack STATIC)
TARGET_C_PROPERTIES (h5repack STATIC " " " ")
target_link_libraries (h5repack  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5repack PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repack")

set (H5_DEP_EXECUTABLES h5repack)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5repack ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5repack
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
