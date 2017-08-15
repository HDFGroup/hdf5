cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
add_executable (h5stat ${HDF5_TOOLS_SRC_H5STAT_SOURCE_DIR}/h5stat.c)
TARGET_NAMING (h5stat STATIC)
TARGET_C_PROPERTIES (h5stat STATIC " " " ")
target_link_libraries (h5stat  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5stat PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5stat")

set (H5_DEP_EXECUTABLES h5stat)

if (BUILD_SHARED_LIBS)
  add_executable (h5stat-shared ${HDF5_TOOLS_SRC_H5STAT_SOURCE_DIR}/h5stat.c)
  TARGET_NAMING (h5stat-shared SHARED)
  TARGET_C_PROPERTIES (h5stat-shared SHARED " " " ")
  target_link_libraries (h5stat-shared  ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
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
  if (BUILD_SHARED_LIBS)
    INSTALL_PROGRAM_PDB (h5stat-shared ${HDF5_INSTALL_BIN_DIR} toolsapplications)
  endif ()
  INSTALL_PROGRAM_PDB (h5stat ${HDF5_INSTALL_BIN_DIR} toolsapplications)

  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolsapplications
  )
endif ()
