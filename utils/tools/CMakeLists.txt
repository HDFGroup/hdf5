cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS C)

add_subdirectory (dh5walk)

#-- Add the tests
if (BUILD_TESTING)
  add_subdirectory (test)
endif()
