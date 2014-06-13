cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_TOOLS)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)

# --------------------------------------------------------------------
# If testing was NOT enabled, then we need to build the tools library
# --------------------------------------------------------------------
if (NOT BUILD_TESTING)
  add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/lib)
endif (NOT BUILD_TESTING)

#-- Add the h5diff and test executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5diff)

#-- Add the h5ls executable
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5ls)

#-- Misc Executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/misc)

#-- Add the h5import and test executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5import)

#-- h5Repack executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5repack)

#-- Add the h5dump and test executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5jam)

#-- Add the h5copy and test executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5copy)

#-- Add the h5stat and test executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5stat)

#-- Add the h5dump and test executables
add_subdirectory (${HDF5_TOOLS_SOURCE_DIR}/h5dump)
