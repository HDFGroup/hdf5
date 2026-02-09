cmake_minimum_required (VERSION 3.26)
PROJECT (HDF5Examples_F90 Fortran)

# Parallel IO usage requires MPI to be linked and included
# Find MPI Fortran components to populate MPI_Fortran_LIBRARIES
if (H5_HAVE_PARALLEL)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  set (H5EXAMPLE_LINK_Fortran_LIBS ${H5EXAMPLE_LINK_Fortran_LIBS} ${MPI_Fortran_LIBRARIES})
  if (MPI_Fortran_LINK_FLAGS)
    set (CMAKE_Fortran_EXE_LINKER_FLAGS "${MPI_Fortran_LINK_FLAGS} ${CMAKE_Fortran_EXE_LINKER_FLAGS}")
  endif ()
endif ()

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
