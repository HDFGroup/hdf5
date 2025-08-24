# CMake configuration for building and testing the h5jam tool
# Adds test executables for getub and tellub, with proper include/linking/formatting
# Includes test scripts if test tools and serial tests are enabled
# Supports both static and shared builds

cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_TEST_H5JAM C)

add_executable (getub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/getub.c)
target_include_directories (getub PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (getub STATIC)
  target_link_libraries (getub PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (getub SHARED)
  target_link_libraries (getub PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (getub PROPERTIES FOLDER tools)

add_executable (tellub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/tellub.c)
target_include_directories (tellub PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (tellub STATIC)
  target_link_libraries (tellub PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (tellub SHARED)
  target_link_libraries (tellub PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (tellub PROPERTIES FOLDER tools)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_H5JAM_getub_FORMAT getub)
  clang_format (HDF5_TOOLS_TEST_H5JAM_tellub_FORMAT tellub)
endif ()

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
