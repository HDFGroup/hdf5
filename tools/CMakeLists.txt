cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS C)

add_subdirectory (lib)

#-- Add the h5diff and test executables
add_subdirectory (h5diff)

#-- Add the h5ls executable
add_subdirectory (h5ls)

#-- Misc Executables
add_subdirectory (misc)

#-- Add the h5import and test executables
add_subdirectory (h5import)

#-- h5repack executables
add_subdirectory (h5repack)

#-- Add the h5dump and test executables
add_subdirectory (h5jam)

#-- Add the h5copy and test executables
add_subdirectory (h5copy)

#-- Add the h5stat and test executables
add_subdirectory (h5stat)

#-- Add the h5dump and test executables
add_subdirectory (h5dump)

#-- Add the perform and test executables
add_subdirectory (perform)

#-- Add the tests
if (BUILD_TESTING)
# --------------------------------------------------------------------
# If S3 or HDFS enabled, then we need to test the tools library
# --------------------------------------------------------------------
  if (HDF5_ENABLE_ROS3_VFD OR HDF5_ENABLE_HDFS)
    add_subdirectory (libtest)
  endif ()
endif ()
