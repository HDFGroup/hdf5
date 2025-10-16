cmake_minimum_required (VERSION 3.26)
project (HDF5Examples_FORTRAN_HL Fortran)

# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
set_directory_properties (PROPERTIES INCLUDE_DIRECTORIES
    "${CMAKE_Fortran_MODULE_DIRECTORY}${H5EXAMPLE_MOD_EXT};${HDF5_F90_BINARY_DIR};${PROJECT_BINARY_DIR};${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Fortran_sourcefiles.cmake)

foreach (example_name ${f03_examples})
  add_executable (${EXAMPLE_VARNAME}_f90_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.F90)
  target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
      PRIVATE
          "-DH5_LIBVER_DIR=${H5_LIBVER_DIR}"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_f90_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EXAMPLE_LINK_Fortran_LIBS})
  set_target_properties (${EXAMPLE_VARNAME}_f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
  if (H5EXAMPLE_BUILD_TESTING)
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
    )
  endif ()
endforeach ()

#if (H5EXAMPLE_ENABLE_F2003)
#  foreach (example_name ${f03examples})
#    add_executable (${EXAMPLE_VARNAME}_f90_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
#    target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
#        PRIVATE
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
#    )
#    if (H5_HAVE_PARALLEL)
#      target_include_directories (${EXAMPLE_VARNAME}_f90_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
#    endif ()
#    target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EXAMPLE_LINK_Fortran_LIBS})
#    if (H5EXAMPLE_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/F03/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()

#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.10")
#  foreach (example_name ${1_10_examples})
#    if (H5EXAMPLE_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12")
#  foreach (example_name ${1_12_examples})
#    if (H5EXAMPLE_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/112/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.14")
#  foreach (example_name ${1_14_examples})
#    if (H5EXAMPLE_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/114/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "2.0")
#  foreach (example_name ${2_0_examples})
#    if (H5EXAMPLE_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/200/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()

#if (HDF5_PROVIDES_TOOLS)
#  foreach (example_name ${f03_examples})
#    add_custom_command (
#        TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#        POST_BUILD
#        COMMAND    ${CMAKE_COMMAND}
#        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
#    )
#  endforeach ()
#endif ()

#if (H5EXAMPLE_ENABLE_F2003)
  foreach (example_name ${f03examples})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
    )
  endforeach ()

#  foreach (example_name ${1_10_examples})
#  endforeach ()
#  foreach (example_name ${1_12_examples})
#  endforeach ()
#  foreach (example_name ${1_14_examples})
#  endforeach ()
#  foreach (example_name ${2_0_examples})
#  endforeach ()
#endif ()

if (H5EXAMPLE_BUILD_TESTING)
  macro (ADD_DUMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME ${EXAMPLE_VARNAME}_f90_${testname} COMMAND $<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES
          DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_REFERENCE=${testname}.tst"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EXAMPLE_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES
          ENVIRONMENT "${CROSSCOMPILING_PATH}"
      )
      if (last_test)
        set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "${EXAMPLE_VARNAME}_f90_${testname}")
      if (HDF5_PROVIDES_TOOLS)
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=${H5EXAMPLE_HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}.ddl.out"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                #-D "TEST_REFERENCE=${testname}.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EXAMPLE_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname} PROPERTIES
            DEPENDS ${last_test}
            ENVIRONMENT "${CROSSCOMPILING_PATH}"
        )
        set (last_test "${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}")
      endif ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND ${CMAKE_COMMAND} -E remove ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${last_test})
    endif ()
  endmacro ()

  foreach (example_name ${f03_examples})
    ADD_DUMP_TEST (${example_name})
  endforeach ()

endif ()
