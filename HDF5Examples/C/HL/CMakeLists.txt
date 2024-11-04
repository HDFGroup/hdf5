cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_C_HL C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (C_sourcefiles.cmake)

foreach (example_name ${common_examples})
  add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
  target_compile_options(${EXAMPLE_VARNAME}_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
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
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
    )
  endif ()
endforeach ()

#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8")
#  foreach (example_name ${1_8_examples})
#    add_executable (${EXAMPLE_VARNAME}_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
#    target_compile_options(${EXAMPLE_VARNAME}_${example_name}
#        PRIVATE
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
#            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
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
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.10")
#  foreach (example_name ${1_10_examples})
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
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "2.0")
#  foreach (example_name ${2_0_examples})
#    if (H5EX_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/200/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()

if (HDF5_BUILD_TOOLS)
  add_custom_command (
      TARGET     ${EXAMPLE_VARNAME}_h5ex_image2
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/image8.txt ${PROJECT_BINARY_DIR}/image8.txt
  )
  add_custom_command (
      TARGET     ${EXAMPLE_VARNAME}_h5ex_image2
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/image24pixel.txt ${PROJECT_BINARY_DIR}/image24pixel.txt
  )
  foreach (example_name ${common_examples})
    if (NOT ${example_name} STREQUAL "h5ex_lite1" AND NOT ${example_name} STREQUAL "h5ex_lite2")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      )
    endif ()
  endforeach ()

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
      #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "2.0")
      #  add_custom_command (
      #      TARGET     ${EXAMPLE_VARNAME}_${example_name}
      #      POST_BUILD
      #      COMMAND    ${CMAKE_COMMAND}
      #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/200/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      #  )
      #endif ()
#  endforeach ()
#  foreach (example_name ${1_10_examples})
#  endforeach ()
#  foreach (example_name ${1_12_examples})
#  endforeach ()
#  foreach (example_name ${1_14_examples})
#  endforeach ()
#  foreach (example_name ${2_0_examples})
#  endforeach ()
endif ()

if (H5EX_BUILD_TESTING)
  macro (ADD_H5_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME ${EXAMPLE_VARNAME}_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
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
            NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
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
    endif ()
  endmacro ()

  macro (ADD_H5_NOCMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME ${EXAMPLE_VARNAME}_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname}-clearall)
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
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
      if (HDF5_BUILD_TOOLS)
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}.ddl.out"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                #-D "TEST_REFERENCE=${testname}.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_${testname})
      endif ()
    endif ()
  endmacro ()

  foreach (example_name ${common_examples})
    if (${example_name} STREQUAL "h5ex_ds1")
      ADD_H5_NOCMP_TEST (${example_name})
    elseif (NOT ${example_name} STREQUAL "h5ex_lite1" AND NOT ${example_name} STREQUAL "h5ex_lite2")
      ADD_H5_TEST (${example_name})
    endif ()
  endforeach ()

  # special test for h5ex_lite1 and h5ex_lite2
  add_test (
      NAME ${EXAMPLE_VARNAME}_h5ex_lite-clearall
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          h5ex_lite1.h5
  )
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME ${EXAMPLE_VARNAME}_h5ex_lite1 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_h5ex_lite1>)
    set_tests_properties (${EXAMPLE_VARNAME}_h5ex_lite1 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_h5ex_lite-clearall)
    add_test (NAME ${EXAMPLE_VARNAME}_h5ex_lite2 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_h5ex_lite2>)
    set_tests_properties (${EXAMPLE_VARNAME}_h5ex_lite2 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_h5ex_lite1)
  else ()
    add_test (
        NAME ${EXAMPLE_VARNAME}_h5ex_lite1
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_h5ex_lite1>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=h5ex_lite1.out"
            -D "TEST_REFERENCE=h5ex_lite1.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_h5ex_lite1 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_h5ex_lite-clearall)
    add_test (
        NAME ${EXAMPLE_VARNAME}_h5ex_lite2
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_h5ex_lite2>"
            -D "TEST_ARGS:STRING="
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=h5ex_lite2.out"
            -D "TEST_REFERENCE=h5ex_lite2.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_h5ex_lite2 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_h5ex_lite1)
  endif ()
endif ()
