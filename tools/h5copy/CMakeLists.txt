cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_H5COPY C)

# --------------------------------------------------------------------
# Add the h5copy and test executables
# --------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5copy ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copy.c)
  target_include_directories (h5copy PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5copy PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5copy STATIC)
  target_link_libraries (h5copy PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5copy PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5copy")

  set (H5_DEP_EXECUTABLES h5copy)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h5copy-shared ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copy.c)
  target_include_directories (h5copy-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5copy-shared PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5copy-shared SHARED)
  target_link_libraries (h5copy-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5copy-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5copy-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5copy-shared)
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_H5COPY_SRC_FORMAT h5copy)
  else ()
    clang_format (HDF5_H5COPY_SRC_FORMAT h5copy-shared)
  endif ()
endif ()

if (BUILD_TESTING)
  if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
    add_executable (h5copygentest ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copygentest.c)
    target_include_directories (h5copygentest PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (h5copygentest STATIC)
    target_link_libraries (h5copygentest PRIVATE ${HDF5_LIB_TARGET})
    set_target_properties (h5copygentest PROPERTIES FOLDER generator/tools)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_H5COPY_FORMAT h5copygentest)
    endif ()

    #add_test (NAME h5copygentest COMMAND $<TARGET_FILE:h5copygentest>)
  endif ()

  #-----------------------------------------------------------------------------
  # If plugin library tests can be tested
  #-----------------------------------------------------------------------------
  if (BUILD_SHARED_LIBS)
    set (H5COPY_TOOL_PLUGIN_LIB_CORENAME         "dynlibcopy")
    set (H5COPY_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5COPY_TOOL_PLUGIN_LIB_CORENAME}")
    set (H5COPY_TOOL_PLUGIN_LIB_TARGET           ${H5COPY_TOOL_PLUGIN_LIB_CORENAME})

    add_library (${H5COPY_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_copy.c)
    target_include_directories(${H5COPY_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${H5COPY_TOOL_PLUGIN_LIB_TARGET} SHARED)
    target_link_libraries (${H5COPY_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${H5COPY_TOOL_PLUGIN_LIB_TARGET} ${H5COPY_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_H5COPY_PLUGIN_FORMAT ${H5COPY_TOOL_PLUGIN_LIB_TARGET})
    endif ()

    # make plugins dir
    file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${H5COPY_TOOL_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
            "$<TARGET_FILE:${H5COPY_TOOL_PLUGIN_LIB_TARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5COPY_TOOL_PLUGIN_LIB_TARGET}>"
    )
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
