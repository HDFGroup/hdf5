cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_C_TUTR C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (C_sourcefiles.cmake)

foreach (example_name ${examples})
  add_executable (${EXAMPLE_VARNAME}_tutr_${example_name} ${PROJECT_SOURCE_DIR}/${example_name}.c)
  target_compile_options(${EXAMPLE_VARNAME}_tutr_${example_name}
      PRIVATE
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_16_API}>:-DH5_USE_16_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_18_API}>:-DH5_USE_18_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_110_API}>:-DH5_USE_110_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_112_API}>:-DH5_USE_112_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_114_API}>:-DH5_USE_114_API>"
          "$<$<BOOL:${${EXAMPLE_VARNAME}_USE_200_API}>:-DH5_USE_200_API>"
  )
  if (H5_HAVE_PARALLEL)
    target_include_directories (${EXAMPLE_VARNAME}_tutr_${example_name} PUBLIC ${MPI_C_INCLUDE_DIRS})
  endif ()
  target_link_libraries (${EXAMPLE_VARNAME}_tutr_${example_name} ${H5EX_HDF5_LINK_LIBS})
endforeach ()


if (H5EX_BUILD_TESTING)
  file (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)

  set (${EXAMPLE_VARNAME}_tutr_CLEANFILES
      Attributes.h5
      btrees_file.h5
      cmprss.h5
      default_file.h5
      dset.h5
      extend.h5
      extlink_prefix_source.h5
      extlink_source.h5
      extlink_target.h5
      group.h5
      groups.h5
      hard_link.h5
      mount1.h5
      mount2.h5
      one_index_file.h5
      only_dspaces_and_attrs_file.h5
      only_huge_mesgs_file.h5
      REF_REG.h5
      refere.h5
      refer_deprec.h5
      refer_extern1.h5
      refer_extern2.h5
      SDS.h5
      SDScompound.h5
      SDSextendible.h5
      Select.h5
      separate_indexes_file.h5
      small_lists_file.h5
      soft_link.h5
      subset.h5
      unix2win.h5
      blue/prefix_target.h5
      red/prefix_target.h5
      u2w/u2w_target.h5
  )

   # Remove any output file left over from previous test run
  add_test (
      NAME ${EXAMPLE_VARNAME}_tutr-clear-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${${EXAMPLE_VARNAME}_tutr_CLEANFILES}
  )
  set_tests_properties (${EXAMPLE_VARNAME}_tutr-clear-objects PROPERTIES
      FIXTURES_SETUP clear_${EXAMPLE_VARNAME}_tutr
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )
  add_test (
      NAME ${EXAMPLE_VARNAME}_tutr-clean-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${${EXAMPLE_VARNAME}_tutr_CLEANFILES}
  )
  set_tests_properties (${EXAMPLE_VARNAME}_tutr-clean-objects PROPERTIES
      FIXTURES_CLEANUP clear_${EXAMPLE_VARNAME}_tutr
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  macro (ADD_H5_TEST testname)
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME ${EXAMPLE_VARNAME}_tutr_${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:${EXAMPLE_VARNAME}_tutr_${testname}>)
    else ()
      add_test (
          NAME ${EXAMPLE_VARNAME}_tutr_${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${EXAMPLE_VARNAME}_tutr_${testname}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
              -P "${H5EX_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (${EXAMPLE_VARNAME}_tutr_${testname} PROPERTIES FIXTURES_REQUIRED clear_${EXAMPLE_VARNAME}_tutr)
    if (last_test)
      set_tests_properties (${EXAMPLE_VARNAME}_tutr_${testname} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_tutr_${testname}")
  endmacro ()

  foreach (example_name ${examples})
    ADD_H5_TEST (${example_name})
  endforeach ()
endif ()
