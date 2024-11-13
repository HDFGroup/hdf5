cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_CXX_TUTR CXX)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (C_sourcefiles.cmake)

if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8")
  foreach (example_name ${examples})
    add_executable (${EXAMPLE_VARNAME}_cpp_ex_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.cpp)
    target_compile_options(${EXAMPLE_VARNAME}_cpp_ex_${example_name}
        PRIVATE
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
    )
    if (H5_HAVE_PARALLEL)
      target_include_directories (${EXAMPLE_VARNAME}_cpp_ex_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
    endif ()
    target_link_libraries (${EXAMPLE_VARNAME}_cpp_ex_${example_name} ${H5EX_HDF5_LINK_LIBS})
  endforeach ()
endif ()

if (H5EX_BUILD_TESTING)
  macro (ADD_H5_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_cpp_ex_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME ${EXAMPLE_VARNAME}_cpp_ex_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_cpp_ex_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_cpp_ex_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_cpp_ex_${testname}-clearall)
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_cpp_ex_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_cpp_ex_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_cpp_ex_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_cpp_ex_${testname}-clearall)
    endif ()
    if (last_test)
      set_tests_properties (${EXAMPLE_VARNAME}_cpp_ex_${testname} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_cpp_ex_${testname}")
  endmacro ()

  foreach (example_name ${examples})
    ADD_H5_TEST (${example_name})
  endforeach ()
endif ()
