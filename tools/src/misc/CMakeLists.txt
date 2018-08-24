cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_SRC_MISC C)

# --------------------------------------------------------------------
# Add the misc executables
# --------------------------------------------------------------------
#-- Misc Executables
add_executable (h5debug ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5debug.c)
target_include_directories(h5debug PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (h5debug STATIC)
target_link_libraries (h5debug PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5debug PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5debug")

add_executable (h5repart ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5repart.c)
target_include_directories(h5repart PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (h5repart STATIC)
target_link_libraries (h5repart PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5repart PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repart")

add_executable (h5mkgrp ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5mkgrp.c)
target_include_directories(h5mkgrp PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (h5mkgrp STATIC)
target_link_libraries (h5mkgrp PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5mkgrp PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5mkgrp")

add_executable (h5clear ${HDF5_TOOLS_SRC_MISC_SOURCE_DIR}/h5clear.c)
target_include_directories(h5clear PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (h5clear STATIC)
target_link_libraries (h5clear PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
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
if (HDF5_EXPORTED_TARGETS)
  foreach (exec ${H5_DEP_EXECUTABLES})
    INSTALL_PROGRAM_PDB (${exec} ${HDF5_INSTALL_BIN_DIR} toolsapplications)
  endforeach ()

  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
  )
endif ()
