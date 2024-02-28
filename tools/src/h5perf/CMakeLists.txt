cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_SRC_H5PERF C)

# --------------------------------------------------------------------
# h5perf_serial
# --------------------------------------------------------------------
set (h5perf_serial_SOURCES
    ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/sio_perf.c
    ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/sio_engine.c
)
add_executable (h5perf_serial ${h5perf_serial_SOURCES})
target_include_directories (h5perf_serial PRIVATE "${HDF5_TEST_SRC_DIR};${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5perf_serial STATIC)
  target_link_libraries (h5perf_serial PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5perf_serial SHARED)
  target_link_libraries (h5perf_serial PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5perf_serial PROPERTIES FOLDER perform)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5perf_serial")

set (H5_DEP_EXECUTABLES h5perf_serial)

if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_SRC_H5PERF_h5perf_serial_FORMAT h5perf_serial)
endif ()

# --------------------------------------------------------------------
# h5perf
# --------------------------------------------------------------------
if (H5_HAVE_PARALLEL)
  set (h5perf_SOURCES
      ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/pio_perf.c
      ${HDF5_TOOLS_SRC_H5PERF_SOURCE_DIR}/pio_engine.c
  )
  add_executable (h5perf ${h5perf_SOURCES})
  target_include_directories (h5perf PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (HDF5_BUILD_STATIC_TOOLS)
    TARGET_C_PROPERTIES (h5perf STATIC)
    target_link_libraries (h5perf PRIVATE ${LINK_LIBS} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET}  "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>")
  else ()
    TARGET_C_PROPERTIES (h5perf SHARED)
    target_link_libraries (h5perf PRIVATE ${LINK_LIBS} ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>")
  endif ()
  set_target_properties (h5perf PROPERTIES FOLDER perform)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5perf")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5perf)

  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_SRC_H5PERF_h5perf_FORMAT h5perf)
  endif ()
endif ()

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
