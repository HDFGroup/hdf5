# CMake configuration for building and testing the h5sign tool
# Creates test executables and scripts for signing plugin files
# Supports both static and shared builds

cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_TEST_H5SIGN C)

# Create a dummy plugin generator for testing
add_executable (h5signgentest ${HDF5_TOOLS_TEST_H5SIGN_SOURCE_DIR}/h5signgentest.c)
target_include_directories (h5signgentest
  PRIVATE
    "${HDF5_TOOLS_ROOT_DIR}/lib"
    "${HDF5_SRC_INCLUDE_DIRS}"
    "${HDF5_SRC_BINARY_DIR}"
    "${HDF5_SRC_DIR}"  # For H5PLsig.h
    $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>
)

if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (h5signgentest STATIC)
  target_link_libraries (h5signgentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5signgentest SHARED)
  target_link_libraries (h5signgentest PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5signgentest PROPERTIES FOLDER generator/tools)

# Create signature verification test executable
add_executable (h5signverifytest ${HDF5_TOOLS_TEST_H5SIGN_SOURCE_DIR}/h5signverifytest.c)
target_include_directories (h5signverifytest
  PRIVATE
    "${HDF5_TOOLS_ROOT_DIR}/lib"
    "${HDF5_SRC_INCLUDE_DIRS}"
    "${HDF5_SRC_BINARY_DIR}"
    "${HDF5_SRC_DIR}"  # For H5PLsig.h and H5PLpkg.h
    $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>
)

if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (h5signverifytest STATIC)
  target_link_libraries (h5signverifytest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5signverifytest SHARED)
  target_link_libraries (h5signverifytest PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
# OpenSSL needed for SHA-256 hash computation in revocation test
if (HDF5_REQUIRE_SIGNED_PLUGINS)
  target_link_libraries (h5signverifytest PRIVATE OpenSSL::Crypto)
endif ()
set_target_properties (h5signverifytest PROPERTIES FOLDER test/tools)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_H5SIGN_gentest_FORMAT h5signgentest)
  clang_format (HDF5_TOOLS_TEST_H5SIGN_verifytest_FORMAT h5signverifytest)
endif ()

#-----------------------------------------------------------------------------
# Add tests
#-----------------------------------------------------------------------------
if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
