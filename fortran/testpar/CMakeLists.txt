cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_FORTRAN_TESTPAR C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for parallel_test
add_executable (parallel_test
    ptest.f90
    hyper.f90
    mdset.f90
)
TARGET_NAMING (parallel_test STATIC)
TARGET_FORTRAN_PROPERTIES (parallel_test STATIC " " " ")
target_link_libraries (parallel_test
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
    ${MPI_Fortran_LIBRARIES}
)
if (WIN32 AND MSVC)
  target_link_libraries (parallel_test "ws2_32.lib")
endif ()
target_include_directories (parallel_test PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
set_target_properties (parallel_test PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (parallel_test PROPERTIES FOLDER test/fortran)

include (CMakeTests.cmake)
