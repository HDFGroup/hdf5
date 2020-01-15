cmake_minimum_required (VERSION 3.10)
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
# Build the Fortran Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  if (HDF5_TEST_SERIAL)
    add_subdirectory (test)
  endif ()
  if (MPI_Fortran_FOUND AND HDF5_TEST_PARALLEL)
      add_subdirectory (testpar)
    endif ()
  endif ()
endif ()
