cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_FORTRAN_TESTS C Fortran)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for hl_f90_tstds
add_executable (hl_f90_tstds tstds.F90)
target_include_directories (hl_f90_tstds PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
TARGET_FORTRAN_PROPERTIES (hl_f90_tstds STATIC)
target_link_libraries (hl_f90_tstds PRIVATE ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_target_properties (hl_f90_tstds PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstds PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS)
  add_executable (hl_f90_tstds-shared tstds.F90)
  target_include_directories (hl_f90_tstds-shared PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
  TARGET_FORTRAN_PROPERTIES (hl_f90_tstds-shared SHARED)
  target_link_libraries (hl_f90_tstds-shared PRIVATE ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET})
  set_target_properties (hl_f90_tstds-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

#-- Adding test for hl_f90_tstlite
add_executable (hl_f90_tstlite tstlite.F90)
target_include_directories (hl_f90_tstlite PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
TARGET_FORTRAN_PROPERTIES (hl_f90_tstlite STATIC)
target_link_libraries (hl_f90_tstlite PRIVATE ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_F90_TEST_LIB_TARGET})
set_target_properties (hl_f90_tstlite PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstlite PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS)
  add_executable (hl_f90_tstlite-shared tstlite.F90)
  target_include_directories (hl_f90_tstlite-shared PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
  TARGET_FORTRAN_PROPERTIES (hl_f90_tstlite-shared SHARED)
  target_link_libraries (hl_f90_tstlite-shared PRIVATE ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_TEST_LIBSH_TARGET})
  set_target_properties (hl_f90_tstlite-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

#-- Adding test for hl_f90_tstimage
add_executable (hl_f90_tstimage tstimage.F90)
target_include_directories (hl_f90_tstimage PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
TARGET_FORTRAN_PROPERTIES (hl_f90_tstimage STATIC)
target_link_libraries (hl_f90_tstimage PRIVATE  ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_target_properties (hl_f90_tstimage PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstimage PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS)
  add_executable (hl_f90_tstimage-shared tstimage.F90)
  target_include_directories (hl_f90_tstimage-shared PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
  TARGET_FORTRAN_PROPERTIES (hl_f90_tstimage-shared SHARED)
  target_link_libraries (hl_f90_tstimage-shared PRIVATE ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET})
  set_target_properties (hl_f90_tstimage-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

#-- Adding test for hl_f90_tsttable
add_executable (hl_f90_tsttable tsttable.F90)
target_include_directories (hl_f90_tsttable PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/static;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
TARGET_FORTRAN_PROPERTIES (hl_f90_tsttable STATIC)
target_link_libraries (hl_f90_tsttable PRIVATE ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_F90_TEST_LIB_TARGET})
set_target_properties (hl_f90_tsttable PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tsttable PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS)
  add_executable (hl_f90_tsttable-shared tsttable.F90)
  target_include_directories (hl_f90_tsttable-shared PRIVATE "${CMAKE_Fortran_MODULE_DIRECTORY}/shared;${HDF5_F90_BINARY_DIR};${HDF5_F90_SRC_DIR}/src")
  TARGET_FORTRAN_PROPERTIES (hl_f90_tsttable-shared SHARED)
  target_link_libraries (hl_f90_tsttable-shared PRIVATE ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_TEST_LIBSH_TARGET})
  set_target_properties (hl_f90_tsttable-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

include (CMakeTests.cmake)
