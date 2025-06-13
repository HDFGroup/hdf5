cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_TEST C)

set (HDF5_TOOLS
  h5copy
  h5diff
  h5dump
  h5format_convert
  h5jam
  h5repack
  h5stat
  h5ls
)

set (HDF5_TOOLS_MISC
  h5repart
  # h5clear gentest requires special exit to leave IDs open, excluded here
  # h5perf does not use gentest like other tools, excluded here
)

if (HDF5_BUILD_GENERATORS)
  # Build main gentest
  add_executable(h5gentest ${HDF5_TOOLS_TEST_SOURCE_DIR}/h5gentest.c)
  target_include_directories (h5gentest PRIVATE
    "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};"
    "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")

  if (HDF5_BUILD_STATIC_TOOLS)
      TARGET_C_PROPERTIES (h5gentest STATIC)
      target_link_libraries (h5gentest PRIVATE ${HDF5_LIB_TARGET})
  else ()
      TARGET_C_PROPERTIES (h5gentest SHARED)
      target_link_libraries (h5gentest PRIVATE ${HDF5_LIBSH_TARGET})
  endif ()

  target_include_directories(h5gentest PRIVATE
    "${HDF5_TOOLS_TEST_SOURCE_DIR}/misc"
  )

  # Build object libraries for gentest
  foreach(tool ${HDF5_TOOLS};${HDF5_TOOLS_MISC})
    if (${tool} IN_LIST HDF5_TOOLS_MISC)
      add_library(${tool}gentest OBJECT ${HDF5_TOOLS_TEST_SOURCE_DIR}/misc/${tool}gentest.c)
      target_include_directories (${tool}gentest PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_TEST_SRC_DIR};${HDF5_TOOLS_TST_DIR};${HDF5_TOOLS_ROOT_DIR}/misc;${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    else()
      add_library(${tool}gentest OBJECT ${HDF5_TOOLS_TEST_SOURCE_DIR}/${tool}/${tool}gentest.c)
      target_include_directories (${tool}gentest PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_TEST_SRC_DIR};${HDF5_TOOLS_TST_DIR};${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    endif()

    if (HDF5_BUILD_STATIC_TOOLS)
        TARGET_C_PROPERTIES(${tool}gentest STATIC)
        target_link_libraries (${tool}gentest PRIVATE
          ${HDF5_LIB_TARGET}
          ${HDF5_TOOLS_LIB_TARGET}
          ${HDF5_REQUIRED_LIBRARIES} # For math lib
        )
    else ()
        TARGET_C_PROPERTIES(${tool}gentest SHARED)
        target_link_libraries (${tool}gentest PRIVATE
          ${HDF5_LIBSH_TARGET}
          ${HDF5_TOOLS_LIBSH_TARGET}
          ${HDF5_REQUIRED_LIBRARIES}
        )
    endif ()

    target_include_directories(h5gentest PRIVATE
      "${HDF5_TOOLS_TEST_SOURCE_DIR}/${tool}"
    )

    target_link_libraries(h5gentest PRIVATE ${tool}gentest)
  endforeach()

  set_target_properties (h5gentest PROPERTIES FOLDER generator/tools)

  #-----------------------------------------------------------------------------
  # Add Targets to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_H5COPY_FORMAT h5gentest)
    foreach(tool ${HDF5_TOOLS})
      string(TOUPPER ${tool} tool_upper)
      clang_format (HDF5_TOOLS_TEST_${tool_upper}_FORMAT ${tool}gentest)
    endforeach()
  endif ()

  #add_test (NAME h5gentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5gentest>)
endif ()

#-- Add the h5diff tests
add_subdirectory (h5diff)

#-- Add the h5ls tests
add_subdirectory (h5ls)

#-- Misc tests
add_subdirectory (misc)

#-- Add the h5import tests
add_subdirectory (h5import)

#-- h5Repack tests
add_subdirectory (h5repack)

#-- Add the h5jam tests
add_subdirectory (h5jam)

#-- Add the h5copy tests
add_subdirectory (h5copy)

#-- Add the h5stat tests
add_subdirectory (h5stat)

#-- Add the h5dump tests
add_subdirectory (h5dump)

#-- Add the h5format_convert and test executables
add_subdirectory (h5format_convert)

#-- Add the perform tests
add_subdirectory (perform)
