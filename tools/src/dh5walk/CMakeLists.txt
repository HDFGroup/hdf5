cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_SRC_DH5WALK C)

# --------------------------------------------------------------------
# Add the dh5walk and test executables
# --------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (dh5walk ${HDF5_TOOLS_SRC_DH5WALK_SOURCE_DIR}/dh5walk.c)
  add_custom_command(
	OUTPUT ${HDF5_TOOLS_DIR}/test/demo_destfiles.test
	COMMAND bash -c ${HDF5_TOOLS_SRC_DH5WALK_SOURCE_DIR}/copy_demo_files.sh
	ARGS ${HDF5_TOOLS_DIR}/test ${CMAKE_BINARY_DIR}/bin
	DEPENDS ${HDF5_TOOLS_SRC_DH5WALK_SOURCE_DIR}/copy_demo_files.sh
  )
#  add_custom_target(generate_demo ALL
#    DEPENDS "${HDF5_TOOLS_DIR}/test/demo_destfiles.test"
#  )
  target_include_directories (dh5walk PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${CIRCLE_INCLUDE_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(dh5walk PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (dh5walk STATIC)
  target_link_libraries (dh5walk PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} ${MFU_LIBRARY} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
  set_target_properties (dh5walk PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};dh5walk")

  set (H5_DEP_EXECUTABLES dh5walk)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (dh5walk-shared ${HDF5_TOOLS_SRC_DH5WALK_SOURCE_DIR}/dh5walk.c)
  target_include_directories (dh5walk-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};${CIRCLE_INCLUDE_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(dh5walk-shared PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (dh5walk-shared SHARED)
  target_link_libraries (dh5walk-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${MFU_LIBRARY} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
  set_target_properties (dh5walk-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};dh5walk-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} dh5walk-shared)
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_DH5WALK_SRC_FORMAT dh5walk)
  else ()
    clang_format (HDF5_DH5WALK_SRC_FORMAT dh5walk-shared)
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
