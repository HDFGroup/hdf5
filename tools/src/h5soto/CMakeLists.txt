cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_SRC_H5SOTO C)

# --------------------------------------------------------------------
# CMake configuration for HDF5 h5soto tool
# --------------------------------------------------------------------

add_executable (h5soto ${HDF5_TOOLS_SRC_H5SOTO_SOURCE_DIR}/h5soto.c)
target_include_directories (h5soto PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
target_compile_options (h5soto PRIVATE "${HDF5_CMAKE_C_FLAGS}")
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5soto STATIC)
  target_link_libraries (h5soto PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5soto SHARED)
  target_link_libraries (h5soto PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5soto PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5soto")

set (H5_DEP_EXECUTABLES h5soto)

if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_H5SOTO_SRC_FORMAT h5soto)
endif ()

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
