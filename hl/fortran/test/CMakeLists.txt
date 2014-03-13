cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_HL_FORTRAN_TESTS C CXX Fortran)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-- Adding test for hl_f90_tstds
add_executable (hl_f90_tstds tstds.f90)
TARGET_NAMING (hl_f90_tstds ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (hl_f90_tstds " " " ")
target_link_libraries (hl_f90_tstds ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_target_properties (hl_f90_tstds PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstds PROPERTIES FOLDER test/hl/fortran)

#-- Adding test for hl_f90_tstlite
add_executable (hl_f90_tstlite tstlite.f90)
TARGET_NAMING (hl_f90_tstlite ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (hl_f90_tstlite " " " ")
target_link_libraries (hl_f90_tstlite ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_target_properties (hl_f90_tstlite PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstlite PROPERTIES FOLDER test/hl/fortran)

#-- Adding test for hl_f90_tstimage
add_executable (hl_f90_tstimage tstimage.f90)
TARGET_NAMING (hl_f90_tstimage ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (hl_f90_tstimage " " " ")
target_link_libraries (hl_f90_tstimage  ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_target_properties (hl_f90_tstimage PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tstimage PROPERTIES FOLDER test/hl/fortran)

#-- Adding test for hl_f90_tsttable
add_executable (hl_f90_tsttable tsttable.f90)
TARGET_NAMING (hl_f90_tsttable ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (hl_f90_tsttable " " " ")
target_link_libraries (hl_f90_tsttable ${HDF5_HL_F90_LIB_TARGET} ${HDF5_F90_LIB_TARGET})
set_target_properties (hl_f90_tsttable PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (hl_f90_tsttable PROPERTIES FOLDER test/hl/fortran)

include (CMakeTests.cmake)
