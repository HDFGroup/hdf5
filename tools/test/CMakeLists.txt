cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST C)

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
