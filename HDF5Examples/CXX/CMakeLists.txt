cmake_minimum_required (VERSION 3.26)
project (HDFCXX_EXAMPLES CXX)

add_subdirectory (H5D)
add_subdirectory (TUTR)

#-- Add High Level Examples
if (H5EXAMPLE_BUILD_HL AND HDF5_PROVIDES_HL_LIB)
  add_subdirectory (HL)
endif ()
