cmake_minimum_required (VERSION 3.12)
project (HDF5Examples_C_H5G C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (C_sourcefiles.cmake)

foreach (example_name ${common_examples})
  if (${H5_LIBVER_DIR} EQUAL 16 OR ${EXAMPLE_VARNAME}_USE_16_API)
    add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/16/${example_name}.c)
  else ()
    add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
  endif ()
  target_compile_options(${EXAMPLE_VARNAME}_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_116_API}>:-DH5_USE_116_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_${example_name} ${H5EX_HDF5_LINK_LIBS})
  if (H5EX_BUILD_TESTING)
    if (NOT ${example_name} STREQUAL "h5ex_g_create")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/16/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
      )
    endif ()
  endif ()
endforeach ()

if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8" AND NOT ${EXAMPLE_VARNAME}_USE_16_API)
  foreach (example_name ${1_8_examples})
    add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
    target_compile_options(${EXAMPLE_VARNAME}_${example_name}
        PRIVATE
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_116_API}>:-DH5_USE_116_API>"
    )
    if (H5_HAVE_PARALLEL)
      target_include_directories (${EXAMPLE_VARNAME}_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
    endif ()
    target_link_libraries (${EXAMPLE_VARNAME}_${example_name} ${H5EX_HDF5_LINK_LIBS})
    if (H5EX_BUILD_TESTING)
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
      )
    endif ()
  endforeach ()
endif ()

#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.10")
#  foreach (example_name ${1_10_examples})
#    add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
#    target_compile_options(${EXAMPLE_VARNAME}_${example_name}
#        PRIVATE
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_116_API}>:-DH5_USE_116_API>"
#    )
#    if (H5_HAVE_PARALLEL)
#      target_include_directories (${EXAMPLE_VARNAME}_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
#    endif ()
#    target_link_libraries (${EXAMPLE_VARNAME}_${example_name} ${H5EX_HDF5_LINK_LIBS})
#    if (H5EX_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12")
#  foreach (example_name ${1_12_examples})
#    if (H5EX_BUILD_TESTING)
#          add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/112/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.14")
#  foreach (example_name ${1_14_examples})
#    if (H5EX_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/114/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.16")
#  foreach (example_name ${1_16_examples})
#    if (H5EX_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/116/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()

if (HDF5_BUILD_TOOLS)
  foreach (example_name ${common_examples})
    if (${example_name} STREQUAL "h5ex_g_create")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/16/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      )
    endif ()
  endforeach ()

  if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8" AND NOT ${${EXAMPLE_VARNAME}_USE_16_API})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/h5ex_g_compact1.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact1.ddl
    )
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_h5ex_g_compact
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/h5ex_g_compact2.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact2.ddl
    )
  endif ()

#  foreach (example_name ${1_8_examples})
    #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8")
    #  add_custom_command (
    #      TARGET     ${EXAMPLE_VARNAME}_${example_name}
    #      POST_BUILD
    #      COMMAND    ${CMAKE_COMMAND}
    #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
    #  )
    #endif ()
    #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.10")
    #  add_custom_command (
    #      TARGET     ${EXAMPLE_VARNAME}_${example_name}
    #      POST_BUILD
    #      COMMAND    ${CMAKE_COMMAND}
    #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
    #  )
    #endif ()
    #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12")
    #  add_custom_command (
    #      TARGET     ${EXAMPLE_VARNAME}_${example_name}
    #      POST_BUILD
    #      COMMAND    ${CMAKE_COMMAND}
    #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/112/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
    #  )
    #endif ()
    #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.14")
    #  add_custom_command (
    #      TARGET     ${EXAMPLE_VARNAME}_${example_name}
    #      POST_BUILD
    #      COMMAND    ${CMAKE_COMMAND}
    #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/114/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
    #  )
    #endif ()
    #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.16")
    #  add_custom_command (
    #      TARGET     ${EXAMPLE_VARNAME}_${example_name}
    #      POST_BUILD
    #      COMMAND    ${CMAKE_COMMAND}
    #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/116/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
    #  )
    #endif ()
#  endforeach ()

#  foreach (example_name ${1_10_examples})
#  endforeach ()
#  foreach (example_name ${1_12_examples})
#  endforeach ()
#  foreach (example_name ${1_14_examples})
#  endforeach ()
#  foreach (example_name ${1_16_examples})
#  endforeach ()
endif ()

if (H5EX_BUILD_TESTING)
  set (exfiles
      h5ex_g_iterate
      h5ex_g_traverse
  )
  if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8" AND NOT ${EXAMPLE_VARNAME}_USE_16_API)
    set (exfiles ${exfiles}
        h5ex_g_visit
    )
  endif ()
  foreach (example ${exfiles})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_${example}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example}.h5 ${PROJECT_BINARY_DIR}/${example}.h5
    )
  endforeach ()

  macro (ADD_DUMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_DUMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_DUMP2_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}1.h5
            ${testname}2.h5
    )
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}1
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${testname}1.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}1.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}1.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname}1 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}2
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
              -D "TEST_ARGS:STRING=${testname}2.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}2.ddl.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${testname}2.ddl"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname}2 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_H5DUMP-${testname}1)
    endif ()
  endmacro ()

  macro (ADD_H5_CMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.out.tmp
    )
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
  endmacro ()

  ADD_DUMP_TEST (h5ex_g_create)
  ADD_H5_CMP_TEST (h5ex_g_iterate)
  ADD_H5_CMP_TEST (h5ex_g_traverse)
  if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8" AND NOT ${EXAMPLE_VARNAME}_USE_16_API)
    ADD_H5_DUMP2_TEST (h5ex_g_compact)
    ADD_H5_CMP_TEST (h5ex_g_corder)
    ADD_H5_CMP_TEST (h5ex_g_phase)
    ADD_H5_CMP_TEST (h5ex_g_intermediate)
    ADD_H5_CMP_TEST (h5ex_g_visit)
  endif ()

endif ()
