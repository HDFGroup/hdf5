cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_SRC_H5STAT C)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5stat ${HDF5_TOOLS_SRC_H5STAT_SOURCE_DIR}/h5stat.c)
  target_include_directories (h5stat PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5stat STATIC)
  target_link_libraries (h5stat PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5stat PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5stat")

  set (H5_DEP_EXECUTABLES h5stat)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h5stat-shared ${HDF5_TOOLS_SRC_H5STAT_SOURCE_DIR}/h5stat.c)
  target_include_directories (h5stat-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5stat-shared SHARED)
  target_link_libraries (h5stat-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5stat-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5stat-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5stat-shared)
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
