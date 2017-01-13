cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-- Add the h5diff tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5diff)

#-- Add the h5ls tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5ls)

#-- Misc tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/misc)

#-- Add the h5import tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5import)

#-- h5Repack tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5repack)

#-- Add the h5jam tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5jam)

#-- Add the h5copy tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5copy)

#-- Add the h5stat tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5stat)

#-- Add the h5dump tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5dump)

#-- Add the h5format_convert and test executables
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/h5format_convert)

#-- Add the perform tests
add_subdirectory (${HDF5_TOOLS_TEST_SOURCE_DIR}/perform)
