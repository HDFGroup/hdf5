cmake_minimum_required (VERSION 3.18)
project (HDF5_HL_TOOLS_H5WATCH C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (H5WATCH_SOURCES
    ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/h5watch.c
)

#-- Add h5watch program
add_executable (h5watch ${H5WATCH_SOURCES})
target_compile_options(h5watch PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_include_directories (h5watch PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (HDF5_BUILD_STATIC_TOOLS)
  TARGET_C_PROPERTIES (h5watch STATIC)
  target_link_libraries (h5watch PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5watch SHARED)
  target_link_libraries (h5watch PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TOOLS_LIBSH_TARGET})
endif ()
set_target_properties (h5watch PROPERTIES FOLDER tools/hl)
set (H5_DEP_EXECUTABLES h5watch)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_HL_TOOLS_H5WATCH_FORMAT h5watch)
endif ()

if (BUILD_TESTING AND HDF5_TEST_SWMR AND HDF5_TEST_SERIAL)
  #-- Add extend_dset program
  set (extend_dset_SOURCES
      ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/extend_dset.c
  )
  add_executable (extend_dset ${extend_dset_SOURCES})
  target_compile_options(extend_dset PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_include_directories (extend_dset PRIVATE "${HDF5_HL_SRC_DIR}/test;${HDF5_HL_SRC_DIR}/src;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (HDF5_BUILD_STATIC_TOOLS)
    TARGET_C_PROPERTIES (extend_dset STATIC)
    target_link_libraries (extend_dset PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (extend_dset SHARED)
    target_link_libraries (extend_dset PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TOOLS_LIBSH_TARGET})
  endif ()
  set_target_properties (extend_dset PROPERTIES FOLDER tools/hl)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_HL_TOOLS_H5WATCH_extend_dset_FORMAT extend_dset)
  endif ()

  add_executable (h5watchgentest ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/h5watchgentest.c)
  target_compile_options(h5watchgentest PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_include_directories (h5watchgentest PRIVATE "${HDF5_HL_SRC_DIR}/src;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (HDF5_BUILD_STATIC_TOOLS)
    TARGET_C_PROPERTIES (h5watchgentest STATIC)
    target_link_libraries (h5watchgentest PRIVATE ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (h5watchgentest SHARED)
    target_link_libraries (h5watchgentest PRIVATE ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (h5watchgentest PROPERTIES FOLDER generator/tools/hl)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_HL_TOOLS_H5WATCH_h5watchgentest_FORMAT h5watchgentest)
  endif ()

  if (HDF5_TEST_TOOLS)
    include (CMakeTests.cmake)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  install (
      TARGETS
          ${H5_DEP_EXECUTABLES}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hltoolsapplications
  )
endif ()
