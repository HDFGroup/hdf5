cmake_minimum_required (VERSION 3.18)
project (HDF5_UTILS_MIRRORVFD C)

#-----------------------------------------------------------------------------
# Add the mirror_server executable
#-----------------------------------------------------------------------------

set (mirror_server_SOURCES
    ${HDF5_UTILS_MIRRORVFD_SOURCE_DIR}/mirror_remote.c
    ${HDF5_UTILS_MIRRORVFD_SOURCE_DIR}/mirror_server.c
    ${HDF5_UTILS_MIRRORVFD_SOURCE_DIR}/mirror_writer.c
    ${HDF5_UTILS_MIRRORVFD_SOURCE_DIR}/mirror_remote.h
)
add_executable (mirror_server ${mirror_server_SOURCES})
target_include_directories (mirror_server PRIVATE "${HDF5_UTILS_DIR};${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (mirror_server STATIC)
  target_link_libraries (mirror_server PRIVATE ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (mirror_server SHARED)
  target_link_libraries (mirror_server PRIVATE ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (mirror_server PROPERTIES FOLDER utils)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};mirror_server")
set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} mirror_server)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_UTILS_MIRRORVFD_SERVER_FORMAT mirror_server)
endif ()

#-----------------------------------------------------------------------------
# Add the mirror_server_stop executable
#-----------------------------------------------------------------------------

set (mirror_server_stop_SOURCES ${HDF5_UTILS_MIRRORVFD_SOURCE_DIR}/mirror_server_stop.c)
add_executable (mirror_server_stop ${mirror_server_stop_SOURCES})
target_include_directories (mirror_server_stop PRIVATE "${HDF5_UTILS_DIR};${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (mirror_server_stop STATIC)
  target_link_libraries (mirror_server_stop PRIVATE ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (mirror_server_stop SHARED)
  target_link_libraries (mirror_server_stop PRIVATE ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (mirror_server_stop PROPERTIES FOLDER utils)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};mirror_server_stop")
set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} mirror_server_stop)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_UTILS_MIRRORVFD_STOP_FORMAT mirror_server_stop)
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
    INSTALL_PROGRAM_PDB (${exec} ${HDF5_INSTALL_BIN_DIR} utilsapplications)
  endforeach ()

  install (
      TARGETS ${H5_DEP_EXECUTABLES}
      EXPORT ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT utilsapplications
  )
endif ()
