cmake_minimum_required (VERSION 3.18)
project (HDF5_UTILS_TOOLS C)


if (HDF5_BUILD_PARALLEL_TOOLS)
   add_subdirectory (h5dwalk)
endif()

#-- Add the tests
if (BUILD_TESTING)
   add_subdirectory (test)
endif()
