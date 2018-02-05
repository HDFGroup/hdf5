cmake_minimum_required (VERSION 3.2.2)
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
add_subdirectory (lib)

#-- Add the h5diff and test executables
add_subdirectory (h5diff)

#-- Add the h5ls executable
add_subdirectory (h5ls)

#-- Misc Executables
add_subdirectory (misc)

#-- Add the h5import and test executables
add_subdirectory (h5import)

#-- h5Repack executables
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
