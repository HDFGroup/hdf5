cmake_minimum_required (VERSION 3.26)
project (HDF5_TOOLS_TEST_H5STAT C)

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
