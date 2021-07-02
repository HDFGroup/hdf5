cmake_minimum_required (VERSION 3.10)
project (HDF5_UTILS C)

add_subdirectory (mirror_vfd)

#-----------------------------------------------------------------------------
# Add the swmr_compat_vfd executable
#-----------------------------------------------------------------------------

set (swmr_compat_vfd_SOURCES ${HDF5_UTILS_SOURCE_DIR}/swmr_compat_vfd.c)
add_executable (swmr_compat_vfd ${swmr_compat_vfd_SOURCES})
target_include_directories (swmr_compat_vfd PRIVATE "${HDF5_UTILS_DIR};${HDF5_SRC_DIR};${HDF5_TEST_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (swmr_compat_vfd STATIC)
  target_link_libraries (swmr_compat_vfd PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (swmr_compat_vfd SHARED)
  target_link_libraries (swmr_compat_vfd PRIVATE ${HDF5_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (swmr_compat_vfd PROPERTIES FOLDER utils)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};swmr_compat_vfd")
set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} swmr_compat_vfd)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_UTILS_FORMAT swmr_compat_vfd)
endif ()

#-----------------------------------------------------------------------------
# Add the vds_elink_compat_vol executable
#-----------------------------------------------------------------------------

set (vds_elink_compat_vol_SOURCES ${HDF5_UTILS_SOURCE_DIR}/vds_elink_compat_vol.c)
add_executable (vds_elink_compat_vol ${vds_elink_compat_vol_SOURCES})
target_include_directories (vds_elink_compat_vol PRIVATE "${HDF5_UTILS_DIR};${HDF5_SRC_DIR};${HDF5_TEST_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (vds_elink_compat_vol STATIC)
  target_link_libraries (vds_elink_compat_vol PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (vds_elink_compat_vol SHARED)
  target_link_libraries (vds_elink_compat_vol PRIVATE ${HDF5_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (vds_elink_compat_vol PROPERTIES FOLDER utils)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};vds_elink_compat_vol")
set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} vds_elink_compat_vol)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_UTILS_FORMAT vds_elink_compat_vol)
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
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT utilsapplications
  )
endif ()
