cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_TEST_H5FC C)

# --------------------------------------------------------------------
# Add the h5format_convert test executables
# --------------------------------------------------------------------
add_executable (h5fc_chk_idx ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/h5fc_chk_idx.c)
target_include_directories (h5fc_chk_idx PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (h5fc_chk_idx STATIC)
  target_link_libraries (h5fc_chk_idx PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5fc_chk_idx SHARED)
  target_link_libraries (h5fc_chk_idx PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5fc_chk_idx PROPERTIES FOLDER tools)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_H5FC_h5fc_chk_idx_FORMAT h5fc_chk_idx)
endif ()

if (HDF5_BUILD_GENERATORS AND BUILD_STATIC_LIBS)
  add_executable (h5fc_gentest ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/h5fc_gentest.c)
  target_include_directories (h5fc_gentest PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5fc_gentest STATIC)
  target_link_libraries (h5fc_gentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5fc_gentest PROPERTIES FOLDER generator/tools)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_H5FC_FORMAT h5fc_gentest)
  endif ()

  #add_test (NAME h5fc_gentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5fc_gentest>)
endif ()

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
