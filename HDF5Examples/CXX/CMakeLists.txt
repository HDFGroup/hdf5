cmake_minimum_required (VERSION 3.18)
project (HDFCXX_EXAMPLES CXX)

add_subdirectory (H5D)
add_subdirectory (TUTR)

#-- Add High Level Examples
if (H5EX_BUILD_HL AND HDF5_BUILD_HL_LIB)
  add_subdirectory (HL)
endif ()
