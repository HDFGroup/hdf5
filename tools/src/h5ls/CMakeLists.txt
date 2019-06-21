cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_SRC_H5LS C)

#-----------------------------------------------------------------------------
# Add the h5ls executable
#-----------------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5ls ${HDF5_TOOLS_SRC_H5LS_SOURCE_DIR}/h5ls.c)
  target_include_directories (h5ls PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5ls STATIC)
  target_link_libraries (h5ls PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5ls PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5ls")

  set (H5_DEP_EXECUTABLES h5ls)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h5ls-shared ${HDF5_TOOLS_SRC_H5LS_SOURCE_DIR}/h5ls.c)
  target_include_directories (h5ls-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5ls-shared SHARED)
  target_link_libraries (h5ls-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5ls-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5ls-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5ls-shared)
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
