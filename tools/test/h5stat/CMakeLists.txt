cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5STAT C)

# --------------------------------------------------------------------
# Add the h5stat test executables
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS AND NOT ONLY_SHARED_LIBS)
  add_executable (h5stat_gentest ${HDF5_TOOLS_TEST_H5STAT_SOURCE_DIR}/h5stat_gentest.c)
  target_include_directories (h5stat_gentest PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5stat_gentest STATIC)
  target_link_libraries (h5stat_gentest PRIVATE ${HDF5_LIB_TARGET})
  set_target_properties (h5stat_gentest PROPERTIES FOLDER generator/tools)

  #add_test (NAME h5stat_gentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5stat_gentest>)
endif ()

include (CMakeTests.cmake)
