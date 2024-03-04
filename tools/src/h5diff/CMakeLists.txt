cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_SRC_H5DIFF C)

# --------------------------------------------------------------------
# Add the h5diff executables
# --------------------------------------------------------------------
add_executable (h5diff
    ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
    ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_main.c
    ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.h
)
target_include_directories (h5diff PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
target_compile_options(h5diff PRIVATE "${HDF5_CMAKE_C_FLAGS}")
#target_compile_definitions (h5diff PRIVATE H5_TOOLS_DEBUG)
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5diff STATIC)
  target_link_libraries (h5diff PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5diff SHARED)
  target_link_libraries (h5diff PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5diff PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5diff")

set (H5_DEP_EXECUTABLES h5diff)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_H5DIFF_SRC_FORMAT h5diff)
endif ()

if (H5_HAVE_PARALLEL)
  add_executable (ph5diff
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/h5diff_common.c
      ${HDF5_TOOLS_SRC_H5DIFF_SOURCE_DIR}/ph5diff_main.c
  )
  target_include_directories (ph5diff PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(ph5diff PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  if (HDF5_BUILD_STATIC_TOOLS)
    TARGET_C_PROPERTIES (ph5diff STATIC)
    target_link_libraries (ph5diff PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>")
  else ()
    TARGET_C_PROPERTIES (ph5diff SHARED)
    target_link_libraries (ph5diff PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>")
  endif ()
  set_target_properties (ph5diff PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};ph5diff")
  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} ph5diff)
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
