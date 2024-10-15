cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_FORTRAN_H5G Fortran)

# --------------------------------------------------------------------
# Notes: When creating examples they should be prefixed
# with "f90_". This allows for easier filtering of the examples.
# --------------------------------------------------------------------

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

foreach (example_name ${common_examples})
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
  target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EX_LINK_Fortran_LIBS})
  set_target_properties (${EXAMPLE_VARNAME}_f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
  if (H5EX_BUILD_TESTING)
    if (NOT ${example_name} STREQUAL "h5ex_g_create" AND NOT ${example_name} STREQUAL "h5ex_g_compact")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
      )
    endif ()
  endif ()
endforeach ()

#if (H5EX_ENABLE_F2003)
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
#    target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EX_LINK_Fortran_LIBS})
#    if (H5EX_BUILD_TESTING)
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
#    if (H5EX_BUILD_TESTING)
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
#    if (H5EX_BUILD_TESTING)
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
#    if (H5EX_BUILD_TESTING)
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
#    if (H5EX_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/200/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()

if (HDF5_BUILD_TOOLS)
  foreach (example_name ${common_examples})
    if (${example_name} STREQUAL "h5ex_g_create")
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      )
    endif ()
  endforeach ()

  add_custom_command (
      TARGET     ${EXAMPLE_VARNAME}_f90_h5ex_g_compact
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/h5ex_g_compact1.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact1.ddl
  )
  add_custom_command (
      TARGET     ${EXAMPLE_VARNAME}_f90_h5ex_g_compact
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/h5ex_g_compact2.ddl ${PROJECT_BINARY_DIR}/h5ex_g_compact2.ddl
  )
endif ()

#if (H5EX_ENABLE_F2003)
#  foreach (example_name ${f03examples})
#    add_custom_command (
#        TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#        POST_BUILD
#        COMMAND    ${CMAKE_COMMAND}
#        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/F03/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
#    )
#  endforeach ()

#  foreach (example_name ${1_10_examples})
#  endforeach ()
#  foreach (example_name ${1_12_examples})
#  endforeach ()
#  foreach (example_name ${1_14_examples})
#  endforeach ()
#  foreach (example_name ${2_0_examples})
#  endforeach ()
#endif ()

if (H5EX_BUILD_TESTING)
  set (exfiles
      h5ex_g_iterate
      h5ex_g_traverse
      h5ex_g_visit
  )
  foreach (example ${exfiles})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_${example}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example}.h5 ${PROJECT_BINARY_DIR}/${example}.h5
    )
  endforeach ()

  macro (ADD_DUMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME ${EXAMPLE_VARNAME}_f90_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      if (last_test)
        set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "${EXAMPLE_VARNAME}_f90_${testname}")
      if (HDF5_BUILD_TOOLS)
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=${ARGN};${testname}.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}.out"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=${testname}.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname} PROPERTIES DEPENDS ${last_test})
        set (last_test "${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}")
      endif ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${last_test})
    endif ()
  endmacro ()

  macro (ADD_H5_DUMP_TEST testname)
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME ${EXAMPLE_VARNAME}_f90_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_REFERENCE=${testname}.tst"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
      if (last_test)
        set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "${EXAMPLE_VARNAME}_f90_${testname}")
      if (HDF5_BUILD_TOOLS)
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}
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
        set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname} PROPERTIES DEPENDS ${last_test})
        set (last_test "${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}")
      endif ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${last_test})
    endif ()
  endmacro ()

  macro (ADD_H5_DUMP2_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
            ${testname}1.h5
            ${testname}2.h5
    )
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME ${EXAMPLE_VARNAME}_f90_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>)
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testname}.h5
      )
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
    else ()
      if (${ARGN} STREQUAL "NULL")
        add_test (
            NAME ${EXAMPLE_VARNAME}_f90_${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                -D "TEST_OUTPUT=${testname}.out"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME ${EXAMPLE_VARNAME}_f90_${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_EXPECT=0"
                -D "TEST_OUTPUT=${testname}.out"
                -D "TEST_REFERENCE=${testname}.tst"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
      if (HDF5_BUILD_TOOLS)
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}1
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=${testname}1.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}1.ddl.out"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=${testname}1.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}1 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
        add_test (
            NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}2
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=${H5EX_HDF5_DUMP_EXECUTABLE}"
                -D "TEST_ARGS:STRING=${testname}2.h5"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=${testname}2.ddl.out"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=${testname}2.ddl"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
                -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}2 PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}1)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_CMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.out.tmp
    )
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME ${EXAMPLE_VARNAME}_f90_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>)
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_f90_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_REFERENCE=${testname}.tst"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clean
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.out.tmp
    )
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname}-clean PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
  endmacro ()

  ADD_H5_DUMP2_TEST (h5ex_g_compact NULL)
  ADD_DUMP_TEST (h5ex_g_create)
  ADD_H5_CMP_TEST (h5ex_g_corder)
  ADD_H5_CMP_TEST (h5ex_g_phase)

  if (HDF5_VERSION_STRING VERSION_GREATER_EQUAL "1.10.0")
    ADD_H5_CMP_TEST (h5ex_g_intermediate)
    ADD_H5_CMP_TEST (h5ex_g_iterate)
    ADD_H5_CMP_TEST (h5ex_g_visit)
    #if (HDF5_VERSION_STRING VERSION_GREATER_EQUAL "1.14.3")
	    #ADD_H5_CMP_TEST (h5ex_g_traverse)
    #endif()
  else ()
    if (H5EX_ENABLE_F2003)
      ADD_H5_CMP_TEST (h5ex_g_intermediate)
      ADD_H5_CMP_TEST (h5ex_g_iterate)
      #  ADD_H5_CMP_TEST (h5ex_g_traverse)
      ADD_H5_CMP_TEST (h5ex_g_visit)
    endif ()
  endif ()

endif ()
