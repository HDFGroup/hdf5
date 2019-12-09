cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_FORTRAN_TESTS C Fortran)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------
set (H5_TESTS
    tstds
    tstlite
    tstimage
    tsttable
)

macro (ADD_H5_FORTRAN_EXE file)
  add_executable (hl_f90_${file} ${file}.F90)
  target_compile_options(hl_f90_${file}
      PRIVATE
          $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
  )
#  set_property(TARGET hl_f90_${file} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#  )
#  set_property(TARGET hl_f90_${file} APPEND PROPERTY
#      LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#  )
  if(MSVC)
    set_property(TARGET hl_f90_${file} PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
  endif()
  if (NOT BUILD_SHARED_LIBS)
    target_include_directories (hl_f90_${file}
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src"
    )
    target_link_libraries (hl_f90_${file}
        PRIVATE
            ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_F90_TEST_LIB_TARGET}
    )
    set_target_properties (hl_f90_${file} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
    )
  else ()
    target_include_directories (hl_f90_${file}
        PRIVATE
            "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src"
    )
    target_link_libraries (hl_f90_${file}
        PRIVATE
            ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_TEST_LIBSH_TARGET}
    )
    set_target_properties (hl_f90_${file} PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
    )
  endif ()
endmacro ()

foreach (h5_test ${H5_TESTS})
  ADD_H5_FORTRAN_EXE(${h5_test})
endforeach ()

include (CMakeTests.cmake)
