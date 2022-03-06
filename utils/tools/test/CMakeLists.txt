cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_TEST C)

#-- Add the h5diff tests
if (HDF5_BUILD_PARALLEL_TOOLS)
   add_subdirectory (h5dwalk)
endif()

