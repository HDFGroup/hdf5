cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_LIB C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------

set (H5_TOOLS_LIB_SOURCES
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_array.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_attr.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_dset.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff_util.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_dump.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_filters.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_ref.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_str.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_type.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_utils.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5trav.c
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/io_timer.c
)

set (H5_TOOLS_LIB_HDRS
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5trav.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_dump.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_utils.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_str.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5tools_ref.h
    ${HDF5_TOOLS_LIB_SOURCE_DIR}/h5diff.h
)

if (NOT ONLY_SHARED_LIBS)
  add_library (${HDF5_TOOLS_LIB_TARGET} STATIC ${H5_TOOLS_LIB_SOURCES} ${H5_TOOLS_LIB_HDRS})
  target_include_directories (${HDF5_TOOLS_LIB_TARGET}
      PRIVATE "${HDF5_TOOLS_LIB_SOURCE_DIR};${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  #target_compile_definitions(${HDF5_TOOLS_LIB_TARGET} PRIVATE H5DIFF_DEBUG>)
  TARGET_C_PROPERTIES (${HDF5_TOOLS_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_TOOLS_LIB_TARGET}
      PUBLIC ${HDF5_LIB_TARGET}
      PRIVATE "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>"
  )
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TOOLS_LIB_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_TOOLS_LIB_TARGET} ${HDF5_TOOLS_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_TOOLS_LIB_TARGET} PROPERTIES FOLDER libraries/tools)
  set (install_targets ${HDF5_TOOLS_LIB_TARGET})
endif ()

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_TOOLS_LIBSH_TARGET} SHARED ${H5_TOOLS_LIB_SOURCES} ${H5_TOOLS_LIB_HDRS})
  target_include_directories (${HDF5_TOOLS_LIBSH_TARGET}
      PRIVATE "${HDF5_TOOLS_LIB_SOURCE_DIR};${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_TOOLS_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      #PRIVATE H5DIFF_DEBUG
  )
  TARGET_C_PROPERTIES (${HDF5_TOOLS_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_TOOLS_LIBSH_TARGET}
      PUBLIC ${HDF5_LIBSH_TARGET}
      PRIVATE "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_LIBRARIES}>"
  )
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TOOLS_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_TOOLS_LIB_NAME} SHARED "TOOLS")
  set_target_properties (${HDF5_TOOLS_LIBSH_TARGET} PROPERTIES FOLDER libraries/tools)
  set (install_targets ${install_targets} ${HDF5_TOOLS_LIBSH_TARGET})
endif ()

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} toolslibraries)
  endif ()
  if (NOT ONLY_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_TOOLS_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} toolslibraries)
  endif ()

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT toolslibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT toolslibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT toolslibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT toolslibraries
      INCLUDES DESTINATION include
  )
endif ()
