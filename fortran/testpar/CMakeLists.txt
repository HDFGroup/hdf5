cmake_minimum_required (VERSION 3.10)
project (HDF5_FORTRAN_TESTPAR C Fortran)

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
target_include_directories (parallel_test
    PRIVATE
        ${TESTPAR_INCLUDES}
)
target_compile_options(parallel_test
    PRIVATE
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_COMPILE_FLAGS}>
)
target_link_libraries (parallel_test
    PRIVATE
        ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_Fortran_LIBS}
        $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"ws2_32.lib">
)
#set_property(TARGET parallel_test APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:"-SUBSYSTEM:CONSOLE">
#)
#set_property(TARGET parallel_test APPEND PROPERTY
#    LINK_FLAGS $<$<STREQUAL:"x${CMAKE_Fortran_SIMULATE_ID}","xMSVC">:${WIN_LINK_FLAGS}>
#)
if(MSVC)
  set_property(TARGET parallel_test PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()
set_target_properties (parallel_test PROPERTIES
    FOLDER test/fortran
    LINKER_LANGUAGE Fortran
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
)
endif ()

include (CMakeTests.cmake)
