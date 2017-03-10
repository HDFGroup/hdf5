cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_HL_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-- Adding test for hl_f90_tstds
add_executable (hl_f90_tstds tstds.F90)
TARGET_NAMING (hl_f90_tstds STATIC)
TARGET_FORTRAN_PROPERTIES (hl_f90_tstds STATIC " " " ")
target_link_libraries (hl_f90_tstds ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
target_include_directories (hl_f90_tstds PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (hl_f90_tstds PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstds PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (hl_f90_tstds-shared tstds.F90)
  TARGET_NAMING (hl_f90_tstds-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (hl_f90_tstds-shared SHARED " " " ")
  target_link_libraries (hl_f90_tstds-shared ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET})
  target_include_directories (hl_f90_tstds-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (hl_f90_tstds-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

#-- Adding test for hl_f90_tstlite
add_executable (hl_f90_tstlite tstlite.F90)
TARGET_NAMING (hl_f90_tstlite STATIC)
TARGET_FORTRAN_PROPERTIES (hl_f90_tstlite STATIC " " " ")
target_link_libraries (hl_f90_tstlite ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_F90_TEST_LIB_TARGET})
target_include_directories (hl_f90_tstlite PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (hl_f90_tstlite PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstlite PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (hl_f90_tstlite-shared tstlite.F90)
  TARGET_NAMING (hl_f90_tstlite-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (hl_f90_tstlite-shared SHARED " " " ")
  target_link_libraries (hl_f90_tstlite-shared ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_TEST_LIBSH_TARGET})
  target_include_directories (hl_f90_tstlite-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (hl_f90_tstlite-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

#-- Adding test for hl_f90_tstimage
add_executable (hl_f90_tstimage tstimage.F90)
TARGET_NAMING (hl_f90_tstimage STATIC)
TARGET_FORTRAN_PROPERTIES (hl_f90_tstimage STATIC " " " ")
target_link_libraries (hl_f90_tstimage  ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
target_include_directories (hl_f90_tstimage PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (hl_f90_tstimage PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstimage PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (hl_f90_tstimage-shared tstimage.F90)
  TARGET_NAMING (hl_f90_tstimage-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (hl_f90_tstimage-shared SHARED " " " ")
  target_link_libraries (hl_f90_tstimage-shared  ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET})
  target_include_directories (hl_f90_tstimage-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (hl_f90_tstimage-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

#-- Adding test for hl_f90_tsttable
add_executable (hl_f90_tsttable tsttable.F90)
TARGET_NAMING (hl_f90_tsttable STATIC)
TARGET_FORTRAN_PROPERTIES (hl_f90_tsttable STATIC " " " ")
target_link_libraries (hl_f90_tsttable ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_F90_TEST_LIB_TARGET})
target_include_directories (hl_f90_tsttable PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (hl_f90_tsttable PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tsttable PROPERTIES FOLDER test/hl/fortran)
if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_executable (hl_f90_tsttable-shared tsttable.F90)
  TARGET_NAMING (hl_f90_tsttable-shared SHARED)
  TARGET_FORTRAN_PROPERTIES (hl_f90_tsttable-shared SHARED " " " ")
  target_link_libraries (hl_f90_tsttable-shared ${HDF5_HL_F90_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_F90_TEST_LIBSH_TARGET})
  target_include_directories (hl_f90_tsttable-shared PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
  set_target_properties (hl_f90_tsttable-shared PROPERTIES
        LINKER_LANGUAGE Fortran
        FOLDER test/hl/fortran
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

include (CMakeTests.cmake)
