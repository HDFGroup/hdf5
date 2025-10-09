cmake_minimum_required (VERSION 3.26)
PROJECT (HDF5Examples_F90 Fortran)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/TUTR)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T)

if (H5_HAVE_PARALLEL AND HDF5_PROVIDES_PARALLEL)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5PAR)
endif ()

#-- Add High Level Examples
if (H5EXAMPLE_BUILD_HL AND HDF5_PROVIDES_HL_LIB)
  add_subdirectory (HL)
endif ()
