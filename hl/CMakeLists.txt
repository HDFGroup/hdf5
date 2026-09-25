cmake_minimum_required (VERSION 3.26)
project (HDF5_HL C)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------

add_subdirectory (src)

# Build HDF5 Tools
if (HDF5_BUILD_TOOLS)
  add_subdirectory (tools)
endif ()

#-- Build the Unit testing if requested
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING AND HDF5_TEST_SERIAL)
  add_subdirectory (test)
endif ()
