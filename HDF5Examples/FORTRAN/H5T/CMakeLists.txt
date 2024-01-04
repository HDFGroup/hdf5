cmake_minimum_required (VERSION 3.12)
project (HDF5Examples_FORTRAN_H5T Fortran)

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

#if (HDF_ENABLE_F2003)
  foreach (example_name ${f03_examples})
    add_executable (${EXAMPLE_VARNAME}_f90_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.F90)
    target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
        PRIVATE
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
            "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_116_API}>:-DH5_USE_116_API>"
    )
    if (H5_HAVE_PARALLEL)
      target_include_directories (${EXAMPLE_VARNAME}_f90_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
    endif ()
    target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EX_LINK_Fortran_LIBS})
    set_target_properties (${EXAMPLE_VARNAME}_f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
    if (H5EX_BUILD_TESTING)
      add_custom_command (
          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
          POST_BUILD
          COMMAND    ${CMAKE_COMMAND}
          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
      )
    endif ()
  endforeach ()
#endif ()

foreach (example_name ${common_examples})
  add_executable (${EXAMPLE_VARNAME}_f90_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.F90)
  target_compile_options(${EXAMPLE_VARNAME}_f90_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_116_API}>:-DH5_USE_116_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_f90_${example_name} PUBLIC ${MPI_Fortran_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_f90_${example_name} ${H5EX_LINK_Fortran_LIBS})
  set_target_properties (${EXAMPLE_VARNAME}_f90_${example_name} PROPERTIES LINKER_LANGUAGE Fortran)
  if (H5EX_BUILD_TESTING)
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
    )
  endif ()
endforeach ()

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
#          add_custom_command (
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
#if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.16")
#  foreach (example_name ${1_16_examples})
#    if (H5EX_BUILD_TESTING)
#      add_custom_command (
#          TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
#          POST_BUILD
#          COMMAND    ${CMAKE_COMMAND}
#          ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/116/${example_name}.tst ${PROJECT_BINARY_DIR}/${example_name}.tst
#      )
#    endif ()
#  endforeach ()
#endif ()

if (HDF5_BUILD_TOOLS)
  foreach (example_name ${common_examples})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
    )
  endforeach ()

  #if (HDF_ENABLE_F2003)
    foreach (example_name ${f03_examples})
      if (NOT ${example_name} STREQUAL "h5ex_t_convert_F03")
        if (${example_name} STREQUAL "h5ex_t_vlen_F03" OR ${example_name} STREQUAL "h5ex_t_vlenatt_F03")
          if (HDF5_VERSION_STRING VERSION_GREATER_EQUAL "1.14.3")
            add_custom_command (
                TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                POST_BUILD
                COMMAND    ${CMAKE_COMMAND}
                ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/114/${example_name}.ddl ${example_name}.ddl
            )
          else ()
            add_custom_command (
                TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                POST_BUILD
                COMMAND    ${CMAKE_COMMAND}
                ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
            )
          endif ()
        elseif ((${example_name} STREQUAL "h5ex_t_objref_F03" OR ${example_name} STREQUAL "h5ex_t_objrefatt_F03") OR (${example_name} STREQUAL "h5ex_t_regref_F03" OR ${example_name} STREQUAL "h5ex_t_regrefatt_F03"))
          if (${EXAMPLE_VARNAME}_USE_16_API OR ${EXAMPLE_VARNAME}_USE_18_API OR ${EXAMPLE_VARNAME}_USE_110_API)
            if (HDF5_VERSION_MAJOR VERSION_EQUAL "1.8")
              if (HDF5_VERSION_STRING VERSION_LESS_EQUAL "1.8.21")
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}21.ddl ${example_name}.ddl
                )
              else ()
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
                )
              endif ()
            elseif (HDF5_VERSION_MAJOR VERSION_EQUAL "1.10")
              if (HDF5_VERSION_STRING VERSION_LESS_EQUAL "1.10.6")
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}06.ddl ${example_name}.ddl
                )
              else ()
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
                )
              endif ()
            elseif (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12")
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/112/${example_name}.ddl ${example_name}.ddl
                )
            else ()
              add_custom_command (
                  TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                  POST_BUILD
                  COMMAND    ${CMAKE_COMMAND}
                  ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
              )
            endif ()
          else ()
            if (HDF5_VERSION_MAJOR VERSION_EQUAL "1.8")
              if (HDF5_VERSION_STRING VERSION_LESS_EQUAL "1.8.21")
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}21.ddl ${example_name}.ddl
                )
              else ()
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
                )
              endif ()
            elseif (HDF5_VERSION_MAJOR VERSION_EQUAL "1.10")
              if (HDF5_VERSION_STRING VERSION_LESS_EQUAL "1.10.6")
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}06.ddl ${example_name}.ddl
                )
              else ()
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
                )
              endif ()
            elseif (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12")
                add_custom_command (
                    TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                    POST_BUILD
                    COMMAND    ${CMAKE_COMMAND}
                    ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/112/${example_name}.ddl ${example_name}.ddl
                )
            else ()
              add_custom_command (
                  TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
                  POST_BUILD
                  COMMAND    ${CMAKE_COMMAND}
                  ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
              )
            endif ()
          endif ()
        else ()
          add_custom_command (
              TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
              POST_BUILD
              COMMAND    ${CMAKE_COMMAND}
              ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/18/${example_name}.ddl ${example_name}.ddl
          )
        endif ()
      endif ()
    endforeach ()
  #endif ()

#  foreach (example_name ${1_10_examples})
      #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.10")
      #  add_custom_command (
      #      TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
      #      POST_BUILD
      #      COMMAND    ${CMAKE_COMMAND}
      #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      #  )
      #endif ()
      #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.12")
      #  add_custom_command (
      #      TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
      #      POST_BUILD
      #      COMMAND    ${CMAKE_COMMAND}
      #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/112/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      #  )
      #endif ()
      #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.14")
      #  add_custom_command (
      #      TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
      #      POST_BUILD
      #      COMMAND    ${CMAKE_COMMAND}
      #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/114/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      #  )
      #endif ()
      #if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.16")
      #  add_custom_command (
      #      TARGET     ${EXAMPLE_VARNAME}_f90_${example_name}
      #      POST_BUILD
      #      COMMAND    ${CMAKE_COMMAND}
      #      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/tfiles/116/${example_name}.ddl ${PROJECT_BINARY_DIR}/${example_name}.ddl
      #  )
      #endif ()
#  endforeach ()
#  foreach (example_name ${1_12_examples})
#  endforeach ()
#  foreach (example_name ${1_14_examples})
#  endforeach ()
#  foreach (example_name ${1_16_examples})
#  endforeach ()
endif ()

if (H5EX_BUILD_TESTING)
  macro (ADD_H5_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
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
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
    if (HDF5_BUILD_TOOLS)
      add_test (
          NAME ${EXAMPLE_VARNAME}_H5DUMP-f90_${testname}
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
      set_tests_properties (${EXAMPLE_VARNAME}_H5DUMP-f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_CMP_TEST testname)
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}-clearall
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${testname}.h5
    )
    add_test (
        NAME ${EXAMPLE_VARNAME}_f90_${testname}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_f90_${testname}>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_EXPECT=0"
            -D "TEST_OUTPUT=${testname}.out"
            -D "TEST_REFERENCE=${testname}.tst"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
    )
    set_tests_properties (${EXAMPLE_VARNAME}_f90_${testname} PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_f90_${testname}-clearall)
  endmacro ()

  macro (TEST_EXAMPLE example)
    if (${example} STREQUAL "h5ex_t_cpxcmpd_F03" OR ${example} STREQUAL "h5ex_t_cpxcmpdatt_F03")
      ADD_H5_TEST (${example} -n)
    elseif (${example} STREQUAL "h5ex_t_convert_F03")
      ADD_H5_CMP_TEST (${example})
    else ()
      ADD_H5_TEST (${example})
    endif ()
  endmacro ()

  #if (HDF_ENABLE_F2003)
    foreach (example_name ${f03_examples} ${common_examples})
      TEST_EXAMPLE (${example_name})
    endforeach ()
  #endif ()
endif ()
