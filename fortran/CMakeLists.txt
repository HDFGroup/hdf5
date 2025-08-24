cmake_minimum_required (VERSION 3.26)
project (HDF5_F90 C Fortran)

message (VERBOSE "Fortran compiler ID is ${CMAKE_Fortran_COMPILER_ID}")
include (${HDF_CONFIG_DIR}/flags/HDFCompilerFortranFlags.cmake)
include (${HDF_CONFIG_DIR}/HDF5UseFortran.cmake)
set (LINK_Fortran_LIBS ${LINK_LIBS})

if (NOT H5_FORTRAN_HAVE_ISO_C_BINDING)
  message (FATAL_ERROR " **** Fortran compiler lacks required Fortran 2003 features; unsupported Fortran 2003 compiler, disable HDF5_BUILD_FORTRAN **** ")
endif ()

# Parallel IO usage requires MPI to be Linked and Included
if (H5_HAVE_PARALLEL)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  set (LINK_Fortran_LIBS ${LINK_Fortran_LIBS} ${MPI_Fortran_LIBRARIES})
  if (MPI_Fortran_LINK_FLAGS)
    set (CMAKE_Fortran_EXE_LINKER_FLAGS "${MPI_Fortran_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
  endif ()
  # Check if MPI-3 Fortran 2008 module mpi_f08 is supported
  if (MPI_Fortran_HAVE_F08_MODULE)
    set (H5_HAVE_MPI_F08 1)
    message (VERBOSE "MPI-3 Fortran 2008 module mpi_f08 is supported")
  else ()
    message (VERBOSE "MPI-3 Fortran 2008 module mpi_f08 is NOT supported")
  endif ()
endif ()

#option (HDF5_INSTALL_MOD_FORTRAN "Copy FORTRAN mod files to include directory (NO SHARED STATIC)" "NO")
set (HDF5_INSTALL_MOD_FORTRAN "SHARED" CACHE STRING "Copy FORTRAN mod files to include directory (NO SHARED STATIC)")
set_property (CACHE HDF5_INSTALL_MOD_FORTRAN PROPERTY STRINGS NO SHARED STATIC)
if (NOT HDF5_INSTALL_MOD_FORTRAN MATCHES "NO")
  if (NOT BUILD_SHARED_LIBS AND BUILD_STATIC_LIBS)
    set (HDF5_INSTALL_MOD_FORTRAN "STATIC")
  elseif (BUILD_SHARED_LIBS AND NOT BUILD_STATIC_LIBS)
    set (HDF5_INSTALL_MOD_FORTRAN "SHARED")
  endif ()
endif ()

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
