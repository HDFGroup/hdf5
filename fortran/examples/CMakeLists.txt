cmake_minimum_required (VERSION 2.8.11)
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
set (examples
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

set (F2003_examples
    rwdset_fortran2003
    nested_derived_type
    compound_fortran2003
    compound_complex_fortran2003
)

foreach (example ${examples})
  add_executable (f90_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  TARGET_NAMING (f90_ex_${example} ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (f90_ex_${example} " " " ")
  if (WIN32)
    set_property (TARGET f90_ex_${example} 
        APPEND PROPERTY COMPILE_DEFINITIONS 
          HDF5F90_WINDOWS
    )
  endif (WIN32)
  target_link_libraries (f90_ex_${example}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (f90_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
  set_target_properties (f90_ex_${example} PROPERTIES FOLDER examples/fortran)
endforeach (example ${examples})

if (HDF5_ENABLE_F2003)
  foreach (example ${F2003_examples})
    add_executable (f03_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
    TARGET_NAMING (f03_ex_${example} ${LIB_TYPE})
    TARGET_FORTRAN_PROPERTIES (f03_ex_${example} " " " ")
    if (WIN32)
      set_property (TARGET f03_ex_${example} 
          APPEND PROPERTY COMPILE_DEFINITIONS HDF5F90_WINDOWS
      )
    endif (WIN32)
    target_link_libraries (f03_ex_${example}
        ${HDF5_F90_LIB_TARGET}
        ${HDF5_LIB_TARGET}
    )
    set_target_properties (f03_ex_${example} PROPERTIES LINKER_LANGUAGE Fortran)
    set_target_properties (f03_ex_${example} PROPERTIES FOLDER examples/fortran03)
  endforeach (example ${F2003_examples})
endif (HDF5_ENABLE_F2003)

if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  add_executable (f90_ex_ph5example ${HDF5_F90_EXAMPLES_SOURCE_DIR}/ph5example.f90)
  TARGET_NAMING (f90_ex_ph5example ${LIB_TYPE})
  TARGET_FORTRAN_PROPERTIES (f90_ex_ph5example " " " ")
  if (WIN32)
    set_property (TARGET f90_ex_ph5example 
        APPEND PROPERTY COMPILE_DEFINITIONS 
          HDF5F90_WINDOWS
    )
  endif (WIN32)
  target_link_libraries (f90_ex_ph5example
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${MPI_Fortran_LIBRARIES}
  )
  set_target_properties (f90_ex_ph5example PROPERTIES LINKER_LANGUAGE Fortran)
  set_target_properties (f90_ex_ph5example PROPERTIES FOLDER examples/fortran)
endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)

if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif (BUILD_TESTING)
