cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_LIBTEST C)

#-----------------------------------------------------------------------------
# Add the h5tools_utils test executables
#-----------------------------------------------------------------------------
add_executable (h5tools_test_utils ${HDF5_TOOLS_LIBTEST_SOURCE_DIR}/h5tools_test_utils.c)
target_compile_options(h5tools_test_utils PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_include_directories(h5tools_test_utils PRIVATE "${HDF5_TOOLS_ROOT_DIR}/lib;${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (h5tools_test_utils STATIC)
  target_link_libraries (h5tools_test_utils PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5tools_test_utils SHARED)
  target_link_libraries (h5tools_test_utils PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${HDF5_TEST_LIBSH_TARGET})
endif ()
set_target_properties (h5tools_test_utils PROPERTIES FOLDER tools)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_H5TOOLS_TEST_SRC_FORMAT h5tools_test_utils)
endif ()

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
