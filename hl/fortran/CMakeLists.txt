cmake_minimum_required (VERSION 3.26)
project (HDF5_HL_F90 C Fortran)

#-----------------------------------------------------------------------------
# Mirroring fortran/CMakeLists.txt so that the HL Fortran targets below pick
# up MPI_Fortran_INCLUDE_DIRS.
#-----------------------------------------------------------------------------
if (H5_HAVE_PARALLEL)
  find_package (MPI REQUIRED COMPONENTS Fortran)
endif ()

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
add_subdirectory (src)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (test)
endif ()
