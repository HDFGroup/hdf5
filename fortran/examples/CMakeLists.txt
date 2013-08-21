cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_F90_EXAMPLES C CXX Fortran)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
    h5_cmprss
    h5_crtdat
    h5_rdwt
    h5_crtatt
    h5_crtgrp
    h5_crtgrpar
    h5_crtgrpd
    h5_extend
    h5_subset
    hyperslab
    selectele
    refobjexample
    refregexample
    mountexample
    compound
)

SET (F2003_examples
    rwdset_fortran2003
    nested_derived_type
    compound_fortran2003
    compound_complex_fortran2003
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (f90_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (f90_ex_${example} ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (f90_ex_${example} " " " ")
  IF (WIN32 AND NOT CYGWIN)
    SET_PROPERTY (TARGET f90_ex_${example} 
        APPEND PROPERTY COMPILE_DEFINITIONS 
          HDF5F90_WINDOWS
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  TARGET_LINK_LIBRARIES (f90_ex_${example}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  SET_TARGET_PROPERTIES (f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
  SET_TARGET_PROPERTIES (f90_ex_${example} PROPERTIES FOLDER examples/fortran)
ENDFOREACH (example ${examples})

IF (HDF5_ENABLE_F2003)
  FOREACH (example ${F2003_examples})
    ADD_EXECUTABLE (f03_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
    TARGET_NAMING (f03_ex_${example} ${LIB_TYPE})
    TARGET_FORTRAN_PROPERTIES (f03_ex_${example} " " " ")
    IF (WIN32 AND NOT CYGWIN)
      SET_PROPERTY (TARGET f03_ex_${example} 
          APPEND PROPERTY COMPILE_DEFINITIONS HDF5F90_WINDOWS
      )
    ENDIF (WIN32 AND NOT CYGWIN)
    TARGET_LINK_LIBRARIES (f03_ex_${example}
        ${HDF5_F90_LIB_TARGET}
        ${HDF5_LIB_TARGET}
    )
    SET_TARGET_PROPERTIES (f03_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
    SET_TARGET_PROPERTIES (f03_ex_${example} PROPERTIES FOLDER examples/fortran03)
  ENDFOREACH (example ${F2003_examples})
ENDIF (HDF5_ENABLE_F2003)

IF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  ADD_EXECUTABLE (f90_ex_ph5example ${HDF5_F90_EXAMPLES_SOURCE_DIR}/ph5example.f90)
  TARGET_NAMING (f90_ex_ph5example ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (f90_ex_ph5example " " " ")
  IF (WIN32 AND NOT CYGWIN)
    SET_PROPERTY (TARGET f90_ex_ph5example 
        APPEND PROPERTY COMPILE_DEFINITIONS 
          HDF5F90_WINDOWS
    )
  ENDIF (WIN32 AND NOT CYGWIN)
  TARGET_LINK_LIBRARIES (f90_ex_ph5example
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${MPI_Fortran_LIBRARIES}
  )
  SET_TARGET_PROPERTIES (f90_ex_ph5example PROPERTIES LINKER_LANGUAGE Fortran)
  SET_TARGET_PROPERTIES (f90_ex_ph5example PROPERTIES FOLDER examples/fortran)
ENDIF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)

IF (BUILD_TESTING)
  INCLUDE (CMakeTests.cmake)
ENDIF (BUILD_TESTING)
