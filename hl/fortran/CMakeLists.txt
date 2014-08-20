cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_HL_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
add_subdirectory (${HDF5_HL_F90_SOURCE_DIR}/src ${HDF5_HL_F90_BINARY_DIR}/src)

#-----------------------------------------------------------------------------
# Build the HL Fortran Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_FORTRAN_EXAMPLES)
  add_subdirectory (${HDF5_HL_F90_SOURCE_DIR}/examples ${HDF5_HL_F90_BINARY_DIR}/examples)
endif (HDF5_BUILD_FORTRAN_EXAMPLES)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (${HDF5_HL_F90_SOURCE_DIR}/test ${HDF5_HL_F90_BINARY_DIR}/test)
endif (BUILD_TESTING)
