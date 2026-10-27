cmake_minimum_required (VERSION 3.18)
project (HDF_DIR_PATHS_RPATH NONE)

set (CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
set (CMAKE_INSTALL_LIBDIR lib CACHE STRING "Install library directory" FORCE)

if (NOT DEFINED HDF5_MACROS_FILE)
  message (FATAL_ERROR "HDF5_MACROS_FILE is required")
endif ()
if (NOT DEFINED HDF5_TEST_INSTALL_BIN_DIR OR NOT DEFINED HDF5_TEST_INSTALL_LIB_DIR)
  message (FATAL_ERROR "Install bin and lib directories are required")
endif ()
if (NOT DEFINED HDF5_TEST_EXPECTED_RELATIVE)
  message (FATAL_ERROR "Expected bin-to-lib relative path is required")
endif ()

set (HDF5_EXTERNALLY_CONFIGURED TRUE)
set (HDF5_PACKAGE_NAME HDF5)
set (HDF5_USE_GNU_DIRS ON)
set (CMAKE_INSTALL_PREFIX "/opt/hdf5-dir-paths-test")
set (HDF5_INSTALL_BIN_DIR "${HDF5_TEST_INSTALL_BIN_DIR}")
set (HDF5_INSTALL_LIB_DIR "${HDF5_TEST_INSTALL_LIB_DIR}")

include ("${HDF5_MACROS_FILE}")
HDF_DIR_PATHS (HDF5)

list (GET CMAKE_INSTALL_RPATH 0 actual_rpath)
if (APPLE)
  set (expected_rpath "@loader_path/${HDF5_TEST_EXPECTED_RELATIVE}")
else ()
  set (expected_rpath "\$ORIGIN/${HDF5_TEST_EXPECTED_RELATIVE}:\$ORIGIN/")
endif ()

if (NOT actual_rpath STREQUAL expected_rpath)
  message (FATAL_ERROR
      "Unexpected install RPATH for bin='${HDF5_INSTALL_BIN_DIR}', "
      "lib='${HDF5_INSTALL_LIB_DIR}': got '${actual_rpath}', "
      "expected '${expected_rpath}'"
  )
endif ()