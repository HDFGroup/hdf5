cmake_minimum_required (VERSION 3.18)
project (HDF5_F90 C Fortran)

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
# Testing
#-----------------------------------------------------------------------------
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)
  add_subdirectory (test)
  if (MPI_Fortran_FOUND)
    add_subdirectory (testpar)
  endif ()
endif ()
