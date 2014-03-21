cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_FORTRAN_TESTPAR C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${CMAKE_Fortran_MODULE_DIRECTORY} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for parallel_test
add_executable (parallel_test
    ptest.f90
    hyper.f90
    mdset.f90
)
TARGET_NAMING (parallel_test ${LIB_TYPE})
TARGET_FORTRAN_PROPERTIES (parallel_test " " " ")
target_link_libraries (parallel_test 
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
    ${MPI_Fortran_LIBRARIES}
)
if (WIN32 AND MSVC)
  target_link_libraries (parallel_test "ws2_32.lib")
endif (WIN32 AND MSVC)
set_target_properties (parallel_test PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (parallel_test PROPERTIES FOLDER test/fortran)

include (CMakeTests.cmake)
