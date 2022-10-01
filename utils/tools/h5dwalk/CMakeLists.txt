cmake_minimum_required (VERSION 3.18)
project (HDF5_UTILS_TOOLS_H5DWALK C)

# --------------------------------------------------------------------
# Add the h5dwalk and test executables
# --------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5dwalk ${HDF5_UTILS_TOOLS_H5DWALK_SOURCE_DIR}/h5dwalk.c)
#  add_custom_target(generate_demo ALL
#    DEPENDS "${HDF5_TOOLS_DIR}/test/demo_destfiles.test"
#  )
  target_include_directories (h5dwalk PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${CIRCLE_INCLUDE_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5dwalk PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5dwalk STATIC)
  target_link_libraries (h5dwalk PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} ${MFU_LIBRARY} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
  set_target_properties (h5dwalk PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5dwalk")

  set (H5_DEP_EXECUTABLES h5dwalk)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h5dwalk-shared ${HDF5_UTILS_TOOLS_H5DWALK_SOURCE_DIR}/h5dwalk.c)
  target_include_directories (h5dwalk-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${CIRCLE_INCLUDE_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5dwalk-shared PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5dwalk-shared SHARED)
  target_link_libraries (h5dwalk-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${MFU_LIBRARY} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>")
  set_target_properties (h5dwalk-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5dwalk-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5dwalk-shared)
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_H5DWALK_SRC_FORMAT h5dwalk)
  else ()
    clang_format (HDF5_H5DWALK_SRC_FORMAT h5dwalk-shared)
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
