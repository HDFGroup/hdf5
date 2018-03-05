cmake_minimum_required (VERSION 3.3.2)
PROJECT (HDF5_F90 C CXX Fortran)

if (H5_HAVE_PARALLEL)
  if (MPI_Fortran_LINK_FLAGS)
    set (CMAKE_EXE_LINKER_FLAGS "${MPI_Fortran_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (src)

#-----------------------------------------------------------------------------
# Build the Fortran Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (test)
  if (MPI_Fortran_FOUND)
    add_subdirectory (testpar)
  endif ()
endif ()
