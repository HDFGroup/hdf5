cmake_minimum_required (VERSION 3.10)
project (HDF5_F90_EXAMPLES C Fortran)
# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_ex_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

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
  target_compile_options(f90_ex_${example}
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
#  set_property(TARGET f90_ex_${example} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET f90_ex_${example} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET f90_ex_${example} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  if (NOT BUILD_SHARED_LIBS)
    target_include_directories (f90_ex_${example}
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
    )
    target_link_libraries (f90_ex_${example}
        PRIVATE
            ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET}
    )
    set_target_properties (f90_ex_${example} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
    )
  else ()
    target_include_directories (f90_ex_${example}
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
    )
    target_link_libraries (f90_ex_${example}
        PRIVATE
            ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET}
    )
    set_target_properties (f90_ex_${example} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
  endif ()
endforeach ()

foreach (example ${F2003_examples})
  add_executable (f03_ex_${example} ${HDF5_F90_EXAMPLES_SOURCE_DIR}/${example}.f90)
  target_compile_options(f03_ex_${example}
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
#  set_property(TARGET f03_ex_${example} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET f03_ex_${example} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET f03_ex_${example} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  if (NOT ONLY_SHARED_LIBS)
    target_include_directories (f03_ex_${example}
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static"
    )
    target_link_libraries (f03_ex_${example}
        PRIVATE
            ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET}
    )
    set_target_properties (f03_ex_${example} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran03
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
    )
  else ()
    target_include_directories (f03_ex_${example}
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared"
    )
    target_link_libraries (f03_ex_${example}
        PRIVATE
            ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET}
    )
    set_target_properties (f03_ex_${example} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran03
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
  endif ()
endforeach ()

if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  add_executable (f90_ex_ph5example ${HDF5_F90_EXAMPLES_SOURCE_DIR}/ph5example.f90)
  target_compile_options(f90_ex_ph5example
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
#  set_property(TARGET f90_ex_ph5example APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET f90_ex_ph5example APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET f90_ex_ph5example PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  if (NOT ONLY_SHARED_LIBS)
    target_include_directories (f90_ex_ph5example
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/static;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
    )
    target_link_libraries (f90_ex_ph5example
        PRIVATE
            ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET}
            $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_LIBRARIES}>
    )
    set_target_properties (f90_ex_ph5example PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
    )
  else ()
    target_include_directories (f90_ex_ph5example
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};${HDF5_F90_BINARY_DIR};${HDF5_F90_BINARY_DIR}/shared;$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_INCLUDE_DIRS}>"
    )
    target_link_libraries (f90_ex_ph5example
        PRIVATE
            ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET}
            $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_Fortran_LIBRARIES}>
    )
    set_target_properties (f90_ex_ph5example PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER examples/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
  endif ()
endif ()

if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif ()
