cmake_minimum_required (VERSION 3.3.2)
PROJECT (HDF5_TOOLS)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

# --------------------------------------------------------------------
# If testing was NOT enabled, then we need to build the tools library
# --------------------------------------------------------------------
add_subdirectory (lib)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)

#-- Add the test sources
add_subdirectory (src)

#-- Add the tests
if (BUILD_TESTING)
  add_subdirectory (test)
endif ()
