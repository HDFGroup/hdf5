cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_LIBTEST C)

#-----------------------------------------------------------------------------
# Add the h5tools_utils test executables
#-----------------------------------------------------------------------------
add_executable (h5tools_test_utils ${HDF5_TOOLS_LIBTEST_SOURCE_DIR}/h5tools_test_utils.c)
target_include_directories(h5tools_utils PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
TARGET_C_PROPERTIES (h5tools_utils STATIC)
target_link_libraries (h5tools_utils PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (h5tools_utils PROPERTIES FOLDER tools)

if (BUILD_SHARED_LIBS)
  add_executable (h5tools_utils-shared ${HDF5_TOOLS_LIBTEST_SOURCE_DIR}/h5tools_utils.c)
  target_include_directories(h5tools_utils-shared PRIVATE "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5tools_utils-shared SHARED)
  target_link_libraries (h5tools_utils-shared PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_target_properties (h5tools_utils-shared PROPERTIES FOLDER tools)
endif ()

include (CMakeTests.cmake)
