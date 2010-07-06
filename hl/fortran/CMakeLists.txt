cmake_minimum_required (VERSION 2.8)
PROJECT(HDF5_HL_F90 C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_F90_SOURCE_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR})
INCLUDE_DIRECTORIES (${HDF5_F90_SRC_BINARY_DIR})
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY})

LINK_DIRECTORIES (${HDF5_SRC_DIR} ${HDF5_FORTRAN_SOURCE_DIR} ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

#-----------------------------------------------------------------------------
# Add debug information (intel Fortran : JB)
#-----------------------------------------------------------------------------
IF (CMAKE_Fortran_COMPILER MATCHES ifort)
    IF (WIN32)
        SET (CMAKE_Fortran_FLAGS_DEBUG "/debug:full /dbglibs " CACHE "flags" STRING FORCE)
        SET (CMAKE_EXE_LINKER_FLAGS_DEBUG "/DEBUG" CACHE "flags" STRING FORCE)
    ENDIF (WIN32)
ENDIF (CMAKE_Fortran_COMPILER MATCHES ifort)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
ADD_SUBDIRECTORY (${HDF5_HL_F90_SOURCE_DIR}/src ${HDF5_HL_F90_BINARY_DIR}/src)

#-----------------------------------------------------------------------------
# Build the HL Fortran Examples
#-----------------------------------------------------------------------------
IF (HDF5_BUILD_FORTRAN_EXAMPLES)
  ADD_SUBDIRECTORY (${HDF5_HL_F90_SOURCE_DIR}/examples ${HDF5_HL_F90_BINARY_DIR}/examples)
ENDIF (HDF5_BUILD_FORTRAN_EXAMPLES)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
IF (BUILD_TESTING)
  ADD_SUBDIRECTORY (${HDF5_HL_F90_SOURCE_DIR}/test ${HDF5_HL_F90_BINARY_DIR}/test)
ENDIF (BUILD_TESTING)
