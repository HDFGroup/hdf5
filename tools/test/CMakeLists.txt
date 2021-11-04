cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_TEST C)

#-- Add the dh5walk tests
if (HDF5_BUILD_PARALLEL_TOOLS AND HDF5_ENABLE_PARALLEL)
  add_subdirectory (dh5walk)
endif()

#-- Add the h5diff tests
add_subdirectory (h5diff)

#-- Add the h5ls tests
add_subdirectory (h5ls)

#-- Misc tests
add_subdirectory (misc)

#-- Add the h5import tests
add_subdirectory (h5import)

#-- h5Repack tests
add_subdirectory (h5repack)

#-- Add the h5jam tests
add_subdirectory (h5jam)

#-- Add the h5copy tests
add_subdirectory (h5copy)

#-- Add the h5stat tests
add_subdirectory (h5stat)

#-- Add the h5dump tests
add_subdirectory (h5dump)

#-- Add the h5format_convert and test executables
add_subdirectory (h5format_convert)

#-- Add the perform tests
add_subdirectory (perform)
