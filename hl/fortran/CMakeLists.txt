cmake_minimum_required (VERSION 3.18)
project (HDF5_HL_F90 C Fortran)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
add_subdirectory (src)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)

  # Temporary workaround for Windows
  if (WIN32)
    set (SAVE_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})
    set (BUILD_SHARED_LIBS OFF)
    if (NOT HDF5_ONLY_SHARED_LIBS)
      add_subdirectory (test)
    endif ()
    set (BUILD_SHARED_LIBS ${SAVE_BUILD_SHARED_LIBS})
  else ()
    add_subdirectory (test)
  endif ()

  if (MPI_Fortran_FOUND)
    add_subdirectory (testpar)
  endif ()
endif ()
