cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS C)

# --------------------------------------------------------------------
# If testing was NOT enabled, then we need to build the tools library
# --------------------------------------------------------------------
add_subdirectory (lib)

#-- Add the test sources
add_subdirectory (src)

#-- Add the tests
if (BUILD_TESTING)
  add_subdirectory (test)
endif ()
