cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_SRC_H5DUMP C)

# --------------------------------------------------------------------
# Add the h5dump executables
# --------------------------------------------------------------------
add_executable (h5dump
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump.c
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_ddl.c
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_xml.c
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump.h
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_defines.h
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_extern.h
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_ddl.h
    ${HDF5_TOOLS_SRC_H5DUMP_SOURCE_DIR}/h5dump_xml.h
)
target_include_directories (h5dump PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
target_compile_options(h5dump PRIVATE "${HDF5_CMAKE_C_FLAGS}")
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5dump STATIC)
  target_link_libraries (h5dump PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5dump SHARED)
  target_link_libraries (h5dump PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5dump PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5dump")

set (H5_DEP_EXECUTABLES h5dump)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_H5DUMP_SRC_FORMAT h5dump)
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
