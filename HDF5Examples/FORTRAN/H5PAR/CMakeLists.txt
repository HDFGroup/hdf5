cmake_minimum_required (VERSION 3.12)
project (HDF5Examples_FORTRAN_H5PAR Fortran)

# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES
  "${CMAKE_Fortran_MODULE_DIRECTORY}${HDF_MOD_EXT};${HDF5_F90_BINARY_DIR};${PROJECT_BINARY_DIR};${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Fortran_sourcefiles.cmake)

foreach (example_name ${examples})
  add_executable (${EXAMPLE_VARNAME}_f90_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.F90)
  target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
      PRIVATE
          "-DH5_LIBVER_DIR=${H5_LIBVER_DIR}"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_116_API}>:-DH5_USE_116_API>"
  )
  target_include_directories (${EXAMPLE_VARNAME}_f90_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EX_LINK_Fortran_LIBS})
  set_target_properties (${EXAMPLE_VARNAME}_f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
endforeach ()

if (H5EX_BUILD_TESTING)
  macro (ADD_GREP_TEST testname mumprocs)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (last_test)
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clearall PROPERTIES DEPENDS ${last_test})
    endif ()
    add_test (NAME ${EXAMPLE_VARNAME}_f90_${testname} COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=${MPIEXEC_EXECUTABLE};${MPIEXEC_NUMPROC_FLAG};${mumprocs};${MPIEXEC_PREFLAGS};$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>;${MPIEXEC_POSTFLAGS}"
        -D "TEST_ARGS:STRING="
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=${testname}.out"
        -D "TEST_REFERENCE:STRING=PHDF5 example finished with no errors"
        -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
        -P "${H5EX_RESOURCES_DIR}/grepTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
    set (last_test "${EXAMPLE_VARNAME}_f90_${testname}")
  endmacro ()

  # Ensure that 24 is a multiple of the number of processes.
  # The number 24 corresponds to SPACE1_DIM1 and SPACE1_DIM2 defined in ph5example.c
  math(EXPR NUMPROCS "24 / ((24 + ${MPIEXEC_MAX_NUMPROCS} - 1) / ${MPIEXEC_MAX_NUMPROCS})")
  foreach (example_name ${examples})
    if (${example_name} STREQUAL "ph5_f90_hyperslab_by_row")
      ADD_GREP_TEST (${example_name} 2)
    elseif (${example_name} STREQUAL "ph5_f90_hyperslab_by_chunk" OR ${example_name} STREQUAL "ph5_f90_hyperslab_by_pattern")
      ADD_GREP_TEST (${example_name} 4)
    else ()
      ADD_GREP_TEST (${example_name} ${NUMPROCS})
    endif ()
  endforeach ()

endif ()
