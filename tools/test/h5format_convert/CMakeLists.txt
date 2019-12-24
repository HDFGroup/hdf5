cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5FC C)

# --------------------------------------------------------------------
# Add the h5format_convert test executables
# --------------------------------------------------------------------
add_executable (h5fc_chk_idx ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/h5fc_chk_idx.c)
target_include_directories (h5fc_chk_idx PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT ONLY_SHARED_LIBS)
  TARGET_C_PROPERTIES (h5fc_chk_idx STATIC)
  target_link_libraries (h5fc_chk_idx PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5fc_chk_idx SHARED)
  target_link_libraries (h5fc_chk_idx PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5fc_chk_idx PROPERTIES FOLDER tools)

if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
  add_executable (h5fc_gentest ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/h5fc_gentest.c)
  target_include_directories (h5fc_gentest PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5fc_gentest STATIC)
  target_link_libraries (h5fc_gentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5fc_gentest PROPERTIES FOLDER generator/tools)

  #add_test (NAME h5fc_gentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5fc_gentest>)
endif ()

include (CMakeTests.cmake)
