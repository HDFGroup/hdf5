cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS C)

add_subdirectory (lib)

#-- Add the test sources
add_subdirectory (src)

#-- Add the tests
if (BUILD_TESTING)
  add_subdirectory (test)

# --------------------------------------------------------------------
# If S3 or HDFS enabled, then we need to test the tools library
# --------------------------------------------------------------------
  if (HDF5_ENABLE_ROS3_VFD OR HDF5_ENABLE_HDFS)
    add_subdirectory (libtest)
  endif ()
endif ()
