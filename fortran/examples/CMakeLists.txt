cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_F90_EXAMPLES C CXX Fortran)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

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
  TARGET_NAMING (f90_ex_${example} STATIC)
  TARGET_FORTRAN_PROPERTIES (f90_ex_${example} STATIC " " " ")
  target_link_libraries (f90_ex_${example}
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  target_include_directories (f90_ex_${example} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
  set_target_properties (f90_ex_${example} PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER examples/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_executable (f90_ex_${example}-shared ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
    TARGET_NAMING (f90_ex_${example}-shared SHARED)
    TARGET_FORTRAN_PROPERTIES (f90_ex_${example}-shared SHARED " " " ")
    target_link_libraries (f90_ex_${example}-shared
        ${HDF5_F90_LIBSH_TARGET}
        ${HDF5_LIBSH_TARGET}
    )
    target_include_directories (f90_ex_${example}-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
    set_target_properties (f90_ex_${example}-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
endforeach (example ${examples})

if (HDF5_ENABLE_F2003)
  foreach (example ${F2003_examples})
    add_executable (f03_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
    TARGET_NAMING (f03_ex_${example} STATIC)
    TARGET_FORTRAN_PROPERTIES (f03_ex_${example} STATIC " " " ")
    target_link_libraries (f03_ex_${example}
        ${HDF5_F90_LIB_TARGET}
        ${HDF5_LIB_TARGET}
    )
    target_include_directories (f03_ex_${example} PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
    set_target_properties (f03_ex_${example} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran03
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
    )
    if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
      add_executable (f03_ex_${example}-shared ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
      TARGET_NAMING (f03_ex_${example}-shared SHARED)
      TARGET_FORTRAN_PROPERTIES (f03_ex_${example}-shared SHARED " " " ")
      target_link_libraries (f03_ex_${example}-shared
          ${HDF5_F90_LIBSH_TARGET}
          ${HDF5_LIBSH_TARGET}
      )
      target_include_directories (f03_ex_${example}-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
      set_target_properties (f03_ex_${example}-shared PROPERTIES
          LINKER_LANGUAGE Fortran
          FOLDER examples/fortran03
          Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
      )
    endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  endforeach (example ${F2003_examples})
endif (HDF5_ENABLE_F2003)

if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  add_executable (f90_ex_ph5example ${HDF5_F90_EXAMPLES_SOURCE_DIR}/ph5example.f90)
  TARGET_NAMING (f90_ex_ph5example STATIC)
  TARGET_FORTRAN_PROPERTIES (f90_ex_ph5example STATIC " " " ")
  target_link_libraries (f90_ex_ph5example
      ${HDF5_F90_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${MPI_Fortran_LIBRARIES}
  )
  target_include_directories (f90_ex_ph5example PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
  set_target_properties (f90_ex_ph5example PROPERTIES
      LINKER_LANGUAGE Fortran
      FOLDER examples/fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
  if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
    add_executable (f90_ex_ph5example-shared ${HDF5_F90_EXAMPLES_SOURCE_DIR}/ph5example.f90)
    TARGET_NAMING (f90_ex_ph5example-shared SHARED)
    TARGET_FORTRAN_PROPERTIES (f90_ex_ph5example-shared SHARED " " " ")
    target_link_libraries (f90_ex_ph5example-shared
        ${HDF5_F90_LIBSH_TARGET}
        ${HDF5_LIBSH_TARGET}
        ${MPI_Fortran_LIBRARIES}
    )
    target_include_directories (f90_ex_ph5example-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
    set_target_properties (f90_ex_ph5example-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
  endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)

if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif (BUILD_TESTING)
