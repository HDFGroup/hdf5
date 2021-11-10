cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS C)


if (HDF5_BUILD_PARALLEL_TOOLS)
   add_subdirectory (dh5walk)
endif()

#-- Add the tests
if (BUILD_TESTING)
   add_subdirectory (test)
endif()
