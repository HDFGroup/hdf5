cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_F90 C CXX Fortran)

if (H5_HAVE_PARALLEL)
  INCLUDE (FindMPI)
  set (LINK_LIBS ${LINK_LIBS} ${MPI_Fortran_LIBRARIES})
  if (MPI_Fortran_LINK_FLAGS)
    set (CMAKE_EXE_LINKER_FLAGS "${MPI_Fortran_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
  endif (MPI_Fortran_LINK_FLAGS)
endif (H5_HAVE_PARALLEL)

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (${HDF5_F90_SOURCE_DIR}/src ${HDF5_F90_BINARY_DIR}/src)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (${HDF5_F90_SOURCE_DIR}/examples ${HDF5_F90_BINARY_DIR}/examples)
endif (HDF5_BUILD_EXAMPLES)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (${HDF5_F90_SOURCE_DIR}/test ${HDF5_F90_BINARY_DIR}/test)
  if (MPI_Fortran_FOUND)
    add_subdirectory (${HDF5_F90_SOURCE_DIR}/testpar ${HDF5_F90_BINARY_DIR}/testpar)
  endif (MPI_Fortran_FOUND)
endif (BUILD_TESTING)
