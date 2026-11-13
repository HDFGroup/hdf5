cmake_minimum_required (VERSION 3.26)
project (HDF5_HL C)

#-----------------------------------------------------------------------------
# Use new-style references with dimension scale APIs; the option itself is
# declared in CMakeBuildOptions.cmake. H5pubconf.h is configured in the
# parent directory, so the define must be set in the parent's scope.
#-----------------------------------------------------------------------------
if (HDF5_DIMENSION_SCALES_NEW_REF)
  set (H5_DIMENSION_SCALES_WITH_NEW_REF 1 PARENT_SCOPE)
endif ()

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
