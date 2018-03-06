cmake_minimum_required (VERSION 3.3.2)
PROJECT (HDF5_FORTRAN_TESTPAR C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
set (TESTPAR_INCLUDES ${MPI_Fortran_INCLUDE_DIRS} ${CMAKE_Fortran_MODULE_DIRECTORY}/static ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for parallel_test
add_executable (parallel_test
    ptest.f90
    hyper.f90
    mdset.f90
)
TARGET_FORTRAN_PROPERTIES (parallel_test STATIC " " " ")
target_link_libraries (parallel_test
    ${HDF5_F90_TEST_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
    ${HDF5_LIB_TARGET}
    ${LINK_Fortran_LIBS}
)
if (WIN32 AND MSVC)
  target_link_libraries (parallel_test "ws2_32.lib")
endif ()
target_include_directories (parallel_test PRIVATE ${TESTPAR_INCLUDES})
set_target_properties (parallel_test PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties (parallel_test PROPERTIES FOLDER test/fortran)

include (CMakeTests.cmake)
