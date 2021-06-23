cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_H5REPACK C)

# --------------------------------------------------------------------
# Add h5Repack executables
# --------------------------------------------------------------------
set (REPACK_COMMON_SOURCES
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_copy.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_filters.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_opttable.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_parse.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_refs.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_verify.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack.c
    ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack.h
)

if (NOT ONLY_SHARED_LIBS)
  add_executable (h5repack ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_main.c)
  target_include_directories (h5repack PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5repack PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5repack STATIC)
  target_link_libraries (h5repack PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5repack PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repack")

  set (H5_DEP_EXECUTABLES h5repack)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h5repack-shared ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repack_main.c)
  target_include_directories (h5repack-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5repack-shared PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5repack-shared SHARED)
  target_link_libraries (h5repack-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5repack-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5repack-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5repack-shared)
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_H5REPACK_SRC_FORMAT h5repack)
  else ()
    clang_format (HDF5_H5REPACK_SRC_FORMAT h5repack-shared)
  endif ()
endif ()

# --------------------------------------------------------------------
# Add h5repack test executables
# --------------------------------------------------------------------
if (BUILD_TESTING)
  add_executable (testh5repack_detect_szip ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testh5repack_detect_szip.c)
  target_include_directories (testh5repack_detect_szip
      PRIVATE "${HDF5_TOOLS_H5REPACK_SOURCE_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  if (NOT ONLY_SHARED_LIBS)
    TARGET_C_PROPERTIES (testh5repack_detect_szip STATIC)
    target_link_libraries (testh5repack_detect_szip PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (testh5repack_detect_szip SHARED)
    target_link_libraries (testh5repack_detect_szip PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (testh5repack_detect_szip PROPERTIES FOLDER tools)

  add_executable (h5repacktest ${REPACK_COMMON_SOURCES} ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repacktst.c)
  target_include_directories (h5repacktest
      PRIVATE "${HDF5_TOOLS_H5REPACK_SOURCE_DIR};${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  if (NOT ONLY_SHARED_LIBS)
    TARGET_C_PROPERTIES (h5repacktest STATIC)
    target_link_libraries (h5repacktest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (h5repacktest SHARED)
    target_link_libraries (h5repacktest PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (h5repacktest PROPERTIES FOLDER tools)

  #-----------------------------------------------------------------------------
  # If plugin library tests can be tested
  #-----------------------------------------------------------------------------
  if (BUILD_SHARED_LIBS)
    set (H5REPACK_TOOL_PLUGIN_LIB_CORENAME         "dynlibadd")
    set (H5REPACK_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5REPACK_TOOL_PLUGIN_LIB_CORENAME}")
    set (H5REPACK_TOOL_PLUGIN_LIB_TARGET           ${H5REPACK_TOOL_PLUGIN_LIB_CORENAME})
    set (H5REPACK_TOOL_PLUGIN_LIB_VCORENAME        "dynlibvers")
    set (H5REPACK_TOOL_PLUGIN_LIB_VNAME            "${HDF5_EXTERNAL_LIB_PREFIX}${H5REPACK_TOOL_PLUGIN_LIB_VCORENAME}")
    set (H5REPACK_TOOL_PLUGIN_LIB_VTARGET          ${H5REPACK_TOOL_PLUGIN_LIB_VCORENAME})

    add_library (${H5REPACK_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_rpk.c)
    target_include_directories (${H5REPACK_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${H5REPACK_TOOL_PLUGIN_LIB_TARGET} SHARED)
    target_link_libraries (${H5REPACK_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${H5REPACK_TOOL_PLUGIN_LIB_TARGET} ${H5REPACK_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

    add_library (${H5REPACK_TOOL_PLUGIN_LIB_VTARGET} SHARED dynlib_vrpk.c)
    target_include_directories (${H5REPACK_TOOL_PLUGIN_LIB_VTARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${H5REPACK_TOOL_PLUGIN_LIB_VTARGET} SHARED)
    target_link_libraries (${H5REPACK_TOOL_PLUGIN_LIB_VTARGET} PRIVATE ${HDF5_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${H5REPACK_TOOL_PLUGIN_LIB_VTARGET} ${H5REPACK_TOOL_PLUGIN_LIB_VNAME} SHARED "LIB")

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_H5REPACK_PLUGIN_FORMAT ${H5REPACK_TOOL_PLUGIN_LIB_TARGET})
      clang_format (HDF5_TOOLS_TEST_H5REPACK_VPLUGIN_FORMAT ${H5REPACK_TOOL_PLUGIN_LIB_VTARGET})
    endif ()

    # make plugins dir
    file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${H5REPACK_TOOL_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
            "$<TARGET_FILE:${H5REPACK_TOOL_PLUGIN_LIB_TARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5REPACK_TOOL_PLUGIN_LIB_TARGET}>"
    )
    add_custom_command (
        TARGET     ${H5REPACK_TOOL_PLUGIN_LIB_VTARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
            "$<TARGET_FILE:${H5REPACK_TOOL_PLUGIN_LIB_VTARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5REPACK_TOOL_PLUGIN_LIB_VTARGET}>"
    )
  endif ()

# --------------------------------------------------------------------
# Add the h5repack test executable
# --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
    add_executable (h5repackgentest ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/h5repackgentest.c)
    target_include_directories (h5repackgentest PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (h5repackgentest STATIC)
    target_link_libraries (h5repackgentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
    set_target_properties (h5repackgentest PROPERTIES FOLDER generator/tools)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_H5REPACK_FORMAT h5repackgentest)
    endif ()

    #add_test (NAME h5repackgentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repackgentest>)
  endif ()

  if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
    include (CMakeTests.cmake)
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
