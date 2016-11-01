cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_TEST_H5LS)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  include (CMakeTests.cmake)

  include (CMakeTestsVDS.cmake)
