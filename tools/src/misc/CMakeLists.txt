cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_MISC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the misc executables
# --------------------------------------------------------------------
#-- Misc Executables
add_executable (h5debug ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5debug.c)
TARGET_NAMING (h5debug STATIC)
TARGET_C_PROPERTIES (h5debug STATIC " " " ")
target_link_libraries (h5debug ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5debug PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5debug")

add_executable (h5repart ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5repart.c)
TARGET_NAMING (h5repart STATIC)
TARGET_C_PROPERTIES (h5repart STATIC " " " ")
target_link_libraries (h5repart ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5repart PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repart")

add_executable (h5mkgrp ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5mkgrp.c)
TARGET_NAMING (h5mkgrp STATIC)
TARGET_C_PROPERTIES (h5mkgrp STATIC " " " ")
target_link_libraries (h5mkgrp  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5mkgrp PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5mkgrp")

add_executable (h5clear ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5clear.c)
TARGET_NAMING (h5clear STATIC)
TARGET_C_PROPERTIES (h5clear STATIC " " " ")
target_link_libraries (h5clear  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5clear PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5clear")

set (H5_DEP_EXECUTABLES
    h5debug
    h5repart
    h5mkgrp
    h5clear
)

#-----------------------------------------------------------------------------
# Generate the h5cc file containing settings needed to compile programs
#-----------------------------------------------------------------------------
#if (NOT WIN32)
#  configure_file (${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5cc.in ${HDF5_BINARY_DIR}/h5cc @ONLY)
#endif ()

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------

#INSTALL_PROGRAM_PDB (h5debug ${HDF5_INSTALL_BIN_DIR} toolsapplications)
#INSTALL_PROGRAM_PDB (h5repart ${HDF5_INSTALL_BIN_DIR} toolsapplications)
#INSTALL_PROGRAM_PDB (h5mkgrp ${HDF5_INSTALL_BIN_DIR} toolsapplications)

install (
    TARGETS
        h5debug h5repart h5mkgrp
    EXPORT
        ${HDF5_EXPORTED_TARGETS}
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
)
