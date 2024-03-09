cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_SRC_H5IMPORT C)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
add_executable (h5import ${HDF5_TOOLS_SRC_H5IMPORT_SOURCE_DIR}/h5import.c ${HDF5_TOOLS_SRC_H5IMPORT_SOURCE_DIR}/h5import.h)
target_include_directories (h5import PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5import STATIC)
  target_link_libraries (h5import PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5import SHARED)
  target_link_libraries (h5import PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
#set_target_properties (h5import PROPERTIES COMPILE_DEFINITIONS H5DEBUGIMPORT)
set_target_properties (h5import PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5import")

set (H5_DEP_EXECUTABLES h5import)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_H5IMPORT_SRC_FORMAT h5import)
endif ()

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
