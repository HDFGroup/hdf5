cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_SRC_H5DIFF C)

# --------------------------------------------------------------------
# Add the h5diff executables
# --------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5diff
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_main.c
  )
  target_include_directories (h5diff PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5diff STATIC)
  target_link_libraries (h5diff PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5diff PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5diff")

  set (H5_DEP_EXECUTABLES h5diff)
endif ()
if (BUILD_SHARED_LIBS)
  add_executable (h5diff-shared
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_main.c
  )
  target_include_directories (h5diff-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5diff-shared SHARED)
  target_link_libraries (h5diff-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5diff-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5diff-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5diff-shared)
endif ()

if (H5_HAVE_PARALLEL)
  if (NOT ONLY_SHARED_LIBS)
    add_executable (ph5diff
        ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
        ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/ph5diff_main.c
    )
    target_include_directories (ph5diff PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (ph5diff STATIC)
    target_link_libraries (ph5diff PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
    set_target_properties (ph5diff PROPERTIES FOLDER tools)
    set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};ph5diff")
    set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} ph5diff)
  endif ()
  if (BUILD_SHARED_LIBS)
    add_executable (ph5diff-shared
        ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
        ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/ph5diff_main.c
    )
    target_include_directories (ph5diff-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (ph5diff-shared SHARED)
    target_link_libraries (ph5diff-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
    set_target_properties (ph5diff-shared PROPERTIES FOLDER tools)
    set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};ph5diff-shared")
    set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} ph5diff-shared)
  endif ()
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
