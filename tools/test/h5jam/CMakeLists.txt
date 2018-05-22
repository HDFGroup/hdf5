cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5JAM C)

# --------------------------------------------------------------------
# Add the h5jam test executables
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS)
  add_executable (h5jamgentest ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/h5jamgentest.c)
  target_include_directories(h5jamgentest PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5jamgentest STATIC)
  target_link_libraries (h5jamgentest PRIVATE ${HDF5_LIB_TARGET})
  set_target_properties (h5jamgentest PROPERTIES FOLDER generator/tools)

  #add_test (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)
endif ()

add_executable (getub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/getub.c)
target_include_directories(getub PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (getub STATIC)
target_link_libraries (getub PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (getub PROPERTIES FOLDER tools)

add_executable (tellub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/tellub.c)
target_include_directories(tellub PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (tellub STATIC)
target_link_libraries (tellub PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (tellub PROPERTIES FOLDER tools)

include (CMakeTests.cmake)
