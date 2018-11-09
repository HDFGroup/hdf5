cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_H5REPACK C)

# --------------------------------------------------------------------
# Add h5Repack test executables
# --------------------------------------------------------------------
if (BUILD_TESTING)
  add_executable (testh5repack_detect_szip ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testh5repack_detect_szip.c)
  target_include_directories(testh5repack_detect_szip
      PRIVATE "${HDF5_TOOLS_SRC_H5REPACK_SOURCE_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  TARGET_C_PROPERTIES (testh5repack_detect_szip STATIC)
  target_link_libraries (testh5repack_detect_szip PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (testh5repack_detect_szip PROPERTIES FOLDER tools)
endif ()

set (REPACK_COMMON_SOURCES
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_copy.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_filters.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_opttable.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_parse.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_refs.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_verify.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack.c
)

add_executable (h5repack ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_main.c)
target_include_directories(h5repack PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (h5repack STATIC)
target_link_libraries (h5repack PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5repack PROPERTIES FOLDER tools)
set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repack")

set (H5_DEP_EXECUTABLES h5repack)

if (BUILD_SHARED_LIBS)
  add_executable (h5repack-shared ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_main.c)
  target_include_directories(h5repack-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5repack-shared SHARED)
  target_link_libraries (h5repack-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5repack-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repack-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5repack-shared)
endif ()

if (BUILD_TESTING)
  add_executable (h5repacktest ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repacktst.c)
  target_include_directories(h5repacktest
      PRIVATE "${HDF5_TOOLS_H5REPACK_SOURCE_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  TARGET_C_PROPERTIES (h5repacktest STATIC)
  target_link_libraries (h5repacktest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (h5repacktest PROPERTIES FOLDER tools)

  #-----------------------------------------------------------------------------
  # If plugin library tests can be tested
  #-----------------------------------------------------------------------------
  if (BUILD_SHARED_LIBS)
    set (HDF5_TOOL_PLUGIN_LIB_CORENAME         "dynlibadd")
    set (HDF5_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_CORENAME}")
    set (HDF5_TOOL_PLUGIN_LIB_TARGET           ${HDF5_TOOL_PLUGIN_LIB_CORENAME})
    set (HDF5_TOOL_PLUGIN_LIB_VCORENAME        "dynlibvers")
    set (HDF5_TOOL_PLUGIN_LIB_VNAME            "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOL_PLUGIN_LIB_VCORENAME}")
    set (HDF5_TOOL_PLUGIN_LIB_VTARGET          ${HDF5_TOOL_PLUGIN_LIB_VCORENAME})

    add_library (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_rpk.c)
    target_include_directories(${HDF5_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${HDF5_TOOL_PLUGIN_LIB_TARGET} SHARED)
    target_link_libraries (${HDF5_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_TOOL_PLUGIN_LIB_TARGET} ${HDF5_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

    add_library (${HDF5_TOOL_PLUGIN_LIB_VTARGET} SHARED dynlib_vrpk.c)
    target_include_directories(${HDF5_TOOL_PLUGIN_LIB_VTARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${HDF5_TOOL_PLUGIN_LIB_VTARGET} SHARED)
    target_link_libraries (${HDF5_TOOL_PLUGIN_LIB_VTARGET} PRIVATE ${HDF5_TEST_LIB_TARGET})
    H5_SET_LIB_OPTIONS (${HDF5_TOOL_PLUGIN_LIB_VTARGET} ${HDF5_TOOL_PLUGIN_LIB_VNAME} SHARED "LIB")

    # make plugins dir
    file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${HDF5_TOOL_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
            "$<TARGET_FILE:${HDF5_TOOL_PLUGIN_LIB_TARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${HDF5_TOOL_PLUGIN_LIB_TARGET}>"
    )
    add_custom_command (
        TARGET     ${HDF5_TOOL_PLUGIN_LIB_VTARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
            "$<TARGET_FILE:${HDF5_TOOL_PLUGIN_LIB_VTARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${HDF5_TOOL_PLUGIN_LIB_VTARGET}>"
    )
  endif ()

  include (CMakeTests.cmake)

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
