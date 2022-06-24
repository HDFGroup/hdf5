add_executable (aux_process aux_process.c)

target_include_directories (aux_process PRIVATE "${HDF5_UITLS_DIR};${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (aux_process STATIC)
  target_link_libraries (aux_process PRIVATE ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (aux_process SHARED)
  target_link_libraries (aux_process PRIVATE ${HDF5_LIBSH_TARGET})
endif ()

set (H5_DEP_EXECUTABLES aux_process)

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
    INSTALL_PROGRAM_PDB (${exec} ${HDF5_INSTALL_BIN_DIR} utilsapplications)
  endforeach ()

  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT utilsapplications
  )
endif ()
