cmake_minimum_required (VERSION 3.10)
project (HDF5_UTILS C)

if (BUILD_TESTING)
  add_subdirectory (test)
endif ()

option (HDF5_BUILD_UTILS  "Build HDF5 Utils" ON)
if (HDF5_BUILD_UTILS)
  add_subdirectory (mirror_vfd)
endif ()
