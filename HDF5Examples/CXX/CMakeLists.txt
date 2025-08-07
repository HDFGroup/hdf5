cmake_minimum_required (VERSION 3.18)
project (HDFCXX_EXAMPLES CXX)

add_subdirectory (H5D)
add_subdirectory (TUTR)

#-- Add High Level Examples
if (H5EX_BUILD_HL AND HDF5_HL_LIB_ENABLED)
  add_subdirectory (HL)
endif ()
