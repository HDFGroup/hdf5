cmake_minimum_required (VERSION 3.18)
project (HDF5_FORTRAN_TESTPAR C Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
set (TESTPAR_INCLUDES ${MPI_Fortran_INCLUDE_DIRS} ${HDF5_F90_BINARY_DIR} ${HDF5_F90_SRC_DIR}/src)
if (NOT BUILD_SHARED_LIBS)
  set (TESTPAR_INCLUDES ${TESTPAR_INCLUDES} ${CMAKE_Fortran_MODULE_DIRECTORY}/static)
else ()
  set (TESTPAR_INCLUDES ${TESTPAR_INCLUDES} ${CMAKE_Fortran_MODULE_DIRECTORY}/shared)
endif ()

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

#-- Adding test for parallel_test
add_executable (parallel_test
    ptest.F90
    hyper.F90
    mdset.F90
    mpi_param.F90
    multidsetrw.F90
)
target_include_directories (parallel_test
    PRIVATE ${TESTPAR_INCLUDES}
)
target_compile_options(parallel_test
    PRIVATE
        "${HDF5_CMAKE_Fortran_FLAGS}"
        "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:${WIN_COMPILE_FLAGS}>"
)
if (NOT BUILD_SHARED_LIBS)
  target_link_libraries (parallel_test
      PRIVATE
          ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_Fortran_LIBS}
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:ws2_32.lib>"
  )
  set_target_properties (parallel_test PROPERTIES
      FOLDER test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
else ()
  target_link_libraries (parallel_test
      PRIVATE
          ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${LINK_Fortran_LIBS}
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:ws2_32.lib>"
  )
  set_target_properties (parallel_test PROPERTIES
      FOLDER test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

if(MSVC)
  set_property(TARGET parallel_test PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()

#-- Adding test for subfiling_test
add_executable (subfiling_test
    subfiling.F90
)
target_include_directories (subfiling_test
    PRIVATE ${TESTPAR_INCLUDES}
)
target_compile_options(subfiling_test
    PRIVATE
        "${HDF5_CMAKE_Fortran_FLAGS}"
        "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:${WIN_COMPILE_FLAGS}>"
)
if (NOT BUILD_SHARED_LIBS)
  target_link_libraries (subfiling_test
      PRIVATE
          ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_Fortran_LIBS}
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:ws2_32.lib>"
  )
  set_target_properties (subfiling_test PROPERTIES
      FOLDER test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
else ()
  target_link_libraries (subfiling_test
      PRIVATE
          ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${LINK_Fortran_LIBS}
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:ws2_32.lib>"
  )
  set_target_properties (subfiling_test PROPERTIES
      FOLDER test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

if(MSVC)
  set_property(TARGET subfiling_test PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()

#-- Adding test for async_test
add_executable (async_test
    async.F90
)
target_include_directories (async_test
    PRIVATE ${TESTPAR_INCLUDES}
)
target_compile_options(async_test
    PRIVATE
        "${HDF5_CMAKE_Fortran_FLAGS}"
        "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:${WIN_COMPILE_FLAGS}>"
)
if (NOT BUILD_SHARED_LIBS)
  target_link_libraries (async_test
      PRIVATE
          ${HDF5_F90_TEST_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_Fortran_LIBS}
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:ws2_32.lib>"
  )
  set_target_properties (async_test PROPERTIES
      FOLDER test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/static
  )
else ()
  target_link_libraries (async_test
      PRIVATE
          ${HDF5_F90_TEST_LIBSH_TARGET} ${HDF5_F90_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} ${LINK_Fortran_LIBS}
          "$<$<STREQUAL:x${CMAKE_Fortran_SIMULATE_ID},xMSVC>:ws2_32.lib>"
  )
  set_target_properties (async_test PROPERTIES
      FOLDER test/fortran
      LINKER_LANGUAGE Fortran
      Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/shared
  )
endif ()

if(MSVC)
  set_property(TARGET async_test PROPERTY LINK_FLAGS "/SUBSYSTEM:CONSOLE ${WIN_LINK_FLAGS}")
endif()

if (HDF5_TEST_FORTRAN AND HDF5_TEST_PARALLEL)
  include (CMakeTests.cmake)
endif ()
