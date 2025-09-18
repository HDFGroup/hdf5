cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_FORTRAN_TUTR Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES
  "${CMAKE_Fortran_MODULE_DIRECTORY}${H5EX_MOD_EXT};${HDF5_F90_BINARY_DIR};${PROJECT_BINARY_DIR};${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Fortran_sourcefiles.cmake)

#if (H5EX_ENABLE_F2003)
  foreach (example_name ${f03_examples})
    add_executable (${EXAMPLE_VARNAME}_f90_tutr_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.f90)
    target_compile_options(${EXAMPLE_VARNAME}_f90_tutr_${example_name}
        PRIVATE
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
    )
    if (H5_HAVE_PARALLEL)
      target_include_directories (${EXAMPLE_VARNAME}_f90_tutr_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
    endif ()
    target_link_libraries (${EXAMPLE_VARNAME}_f90_tutr_${example_name} ${H5EX_LINK_Fortran_LIBS})
    set_target_properties (${EXAMPLE_VARNAME}_f90_tutr_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
  endforeach ()
#endif ()

foreach (example_name ${common_examples})
  add_executable (${EXAMPLE_VARNAME}_f90_tutr_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.f90)
  target_compile_options(${EXAMPLE_VARNAME}_f90_tutr_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_f90_tutr_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_f90_tutr_${example_name} ${H5EX_LINK_Fortran_LIBS})
  set_target_properties (${EXAMPLE_VARNAME}_f90_tutr_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
endforeach ()

if (H5EX_BUILD_TESTING)
  set (${EXAMPLE_VARNAME}_f90_tutr_CLEANFILES
      compound.h5
      copy1.h5
      copy2.h5
      dsetf.h5
      extend.h5
      FORTRAN.h5
      groupf.h5
      groupsf.h5
      h5_cmprss.h5
      mount1.h5
      mount2.h5
      sdsf.h5
      subset.h5
      SDScompound.h5
      test.h5
  )

  # Remove any output file left over from previous test run
  add_test (
      NAME ${EXAMPLE_VARNAME}_f90_tutr-clear-objects
      COMMAND ${CMAKE_COMMAND} -E remove ${${EXAMPLE_VARNAME}_f90_tutr_CLEANFILES}
  )
  set_tests_properties (${EXAMPLE_VARNAME}_f90_tutr-clear-objects PROPERTIES
      FIXTURES_SETUP clear_${EXAMPLE_VARNAME}_f90_tutr
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
  add_test (
      NAME ${EXAMPLE_VARNAME}_f90_tutr-clean-objects
      COMMAND ${CMAKE_COMMAND} -E remove ${${EXAMPLE_VARNAME}_f90_tutr_CLEANFILES}
  )
  set_tests_properties (${EXAMPLE_VARNAME}_f90_tutr-clean-objects PROPERTIES
      FIXTURES_CLEANUP clear_${EXAMPLE_VARNAME}_f90_tutr
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
  macro (ADD_H5_TEST testname)
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME ${EXAMPLE_VARNAME}_f90_tutr_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_f90_tutr_${testname}>)
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_tutr_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_tutr_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (${EXAMPLE_VARNAME}_f90_tutr_${testname} PROPERTIES FIXTURES_REQUIRED clear_${EXAMPLE_VARNAME}_f90_tutr)
    if (last_test)
      set_tests_properties (${EXAMPLE_VARNAME}_f90_tutr_${testname} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_f90_tutr_${testname}")
  endmacro ()

  #if (H5EX_ENABLE_F2003)
    foreach (example_name ${f03_examples} ${common_examples})
      ADD_H5_TEST (${example_name})
    endforeach ()
  #endif ()
endif ()
