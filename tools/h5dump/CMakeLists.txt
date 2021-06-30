cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_H5DUMP C)

# --------------------------------------------------------------------
# Add the h5dump executables
# --------------------------------------------------------------------
if (NOT ONLY_SHARED_LIBS)
  add_executable (h5dump
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.c
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_ddl.c
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_xml.c
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_defines.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_extern.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_ddl.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_xml.h
  )
  target_include_directories (h5dump PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5dump PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5dump STATIC)
  target_link_libraries (h5dump PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  #target_compile_definitions(h5dump PRIVATE H5_TOOLS_DEBUG)
  set_target_properties (h5dump PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5dump")

  set (H5_DEP_EXECUTABLES h5dump)
endif ()

if (BUILD_SHARED_LIBS)
  add_executable (h5dump-shared
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.c
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_ddl.c
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_xml.c
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_defines.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_extern.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_ddl.h
      ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dump_xml.h
  )
  target_include_directories (h5dump-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  target_compile_options(h5dump-shared PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  TARGET_C_PROPERTIES (h5dump-shared SHARED)
  target_link_libraries (h5dump-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  #target_compile_definitions(h5dump-shared PRIVATE H5_TOOLS_DEBUG)
  set_target_properties (h5dump-shared PROPERTIES FOLDER tools)
  set_global_variable (HDF5_UTILS_TO_EXPORT "${HDF5_UTILS_TO_EXPORT};h5dump-shared")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES} h5dump-shared)
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (NOT ONLY_SHARED_LIBS)
    clang_format (HDF5_H5DUMP_SRC_FORMAT h5dump)
  else ()
    clang_format (HDF5_H5DUMP_SRC_FORMAT h5dump-shared)
  endif ()
endif ()

if (BUILD_TESTING)
  if (BUILD_SHARED_LIBS)
    #-----------------------------------------------------------------------------
    # If plugin library tests can be tested
    #-----------------------------------------------------------------------------
    set (H5DUMP_TOOL_PLUGIN_LIB_CORENAME         "dynlibdump")
    set (H5DUMP_TOOL_PLUGIN_LIB_NAME             "${HDF5_EXTERNAL_LIB_PREFIX}${H5DUMP_TOOL_PLUGIN_LIB_CORENAME}")
    set (H5DUMP_TOOL_PLUGIN_LIB_TARGET           ${H5DUMP_TOOL_PLUGIN_LIB_CORENAME})

    add_library (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} SHARED dynlib_dump.c)
    target_include_directories (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} PRIVATE "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} SHARED)
    target_link_libraries (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} PRIVATE ${HDF5_LIBSH_TARGET})
    H5_SET_LIB_OPTIONS (${H5DUMP_TOOL_PLUGIN_LIB_TARGET} ${H5DUMP_TOOL_PLUGIN_LIB_NAME} SHARED "LIB")

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_H5DUMP_PLUGIN_FORMAT ${H5DUMP_TOOL_PLUGIN_LIB_TARGET})
    endif ()

    # make plugins dir
    file (MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/plugins")
    #-----------------------------------------------------------------------------
    # Copy plugin library to a plugins folder
    #-----------------------------------------------------------------------------
    add_custom_command (
        TARGET     ${H5DUMP_TOOL_PLUGIN_LIB_TARGET}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different
            "$<TARGET_FILE:${H5DUMP_TOOL_PLUGIN_LIB_TARGET}>"
            "${CMAKE_BINARY_DIR}/plugins/$<TARGET_FILE_NAME:${H5DUMP_TOOL_PLUGIN_LIB_TARGET}>"
    )
  endif ()

  # --------------------------------------------------------------------
  # Add the h5dump test executable
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
    add_executable (h5dumpgentest ${HDF5_TOOLS_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
    target_include_directories (h5dumpgentest PRIVATE "${HDF5_SRC_DIR};${HDF5_TEST_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    TARGET_C_PROPERTIES (h5dumpgentest STATIC)
    target_link_libraries (h5dumpgentest PRIVATE ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5dumpgentest PROPERTIES FOLDER generator/tools)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_TOOLS_TEST_H5DUMP_FORMAT h5dumpgentest)
    endif ()

    #add_test (NAME h5dumpgentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dumpgentest>)
  endif ()

  if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
    include (CMakeTests.cmake)

    include (CMakeTestsPBITS.cmake)

    include (CMakeTestsXML.cmake)
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
