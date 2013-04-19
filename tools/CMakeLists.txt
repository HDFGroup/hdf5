cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TOOLS)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)

# --------------------------------------------------------------------
# If testing was NOT enabled, then we need to build the tools library
# --------------------------------------------------------------------
IF (NOT BUILD_TESTING)
  ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/lib)
ENDIF (NOT BUILD_TESTING)

#-- Add the h5diff and test executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5diff)

#-- Add the h5ls executable
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5ls)

#-- Misc Executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/misc)

#-- Add the h5import and test executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5import)

#-- h5Repack executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5repack)

#-- Add the h5dump and test executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5jam)

#-- Add the h5copy and test executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5copy)

#-- Add the h5stat and test executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5stat)

#-- Add the h5dump and test executables
ADD_SUBDIRECTORY (${HDF5_TOOLS_SOURCE_DIR}/h5dump)
