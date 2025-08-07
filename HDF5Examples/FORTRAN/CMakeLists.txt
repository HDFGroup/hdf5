cmake_minimum_required (VERSION 3.18)
PROJECT (HDF5Examples_F90 Fortran)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
add_subdirectory (${PROJECT_SOURCE_DIR}/TUTR)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5D)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5G)
add_subdirectory (${PROJECT_SOURCE_DIR}/H5T)

if (H5_HAVE_PARALLEL AND HDF5_PARALLEL_ENABLED)
  add_subdirectory (${PROJECT_SOURCE_DIR}/H5PAR)
endif ()

#-- Add High Level Examples
if (H5EX_BUILD_HL AND HDF5_HL_LIB_ENABLED)
  add_subdirectory (HL)
endif ()
