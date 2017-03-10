cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_SRC)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-- Add the h5diff and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5diff)

#-- Add the h5ls executable
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5ls)

#-- Misc Executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/misc)

#-- Add the h5import and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5import)

#-- h5Repack executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5repack)

#-- Add the h5dump and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5jam)

#-- Add the h5copy and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5copy)

#-- Add the h5stat and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5stat)

#-- Add the h5dump and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5dump)

#-- Add the h5format_convert and test executables
add_subdirectory (${HDF5_TOOLS_SRC_SOURCE_DIR}/h5format_convert)
