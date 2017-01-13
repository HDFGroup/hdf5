
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # Copy all the test files from source directory to test directory
  # --------------------------------------------------------------------
  set (LIST_HDF5_TEST_FILES
      1_a.h5
      1_b.h5
      1_c.h5
      1_d.h5
      1_e.h5
      1_f.h5
      1_vds.h5
      2_a.h5
      2_b.h5
      2_c.h5
      2_d.h5
      2_e.h5
      2_vds.h5
      3_1_vds.h5
      3_2_vds.h5
      4_0.h5
      4_1.h5
      4_2.h5
      4_vds.h5
      5_a.h5
      5_b.h5
      5_c.h5
      5_vds.h5
  )

  set (LIST_OTHER_TEST_FILES
      tvds-1.ls
      tvds-2.ls
      tvds-3_1.ls
      tvds-3_2.ls
      tvds-4.ls
      tvds-5.ls
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  # copy the list of test files
  foreach (listfiles ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
    get_filename_component(fname "${listfiles}" NAME)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/vds/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}" "h5ls_vds_files")
  endforeach ()
  add_custom_target(h5ls_vds_files ALL COMMENT "Copying files needed by h5ls_vds tests" DEPENDS ${h5ls_vds_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_VDS_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5LS-${resultfile} COMMAND $<TARGET_FILE:h5ls> ${ARGN})
      set_tests_properties (H5LS-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
      if (${resultcode} STREQUAL "1")
        set_tests_properties (H5LS-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (H5LS-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      add_test (
          NAME H5LS-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/vds"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  ENDMACRO ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
      NAME H5LS_VDS-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          tvds-1.out
          tvds-1.out.err
          tvds-2.out
          tvds-2.out.err
          tvds-3_1.out
          tvds-3_1.out.err
          tvds-3_2.out
          tvds-3_2.out.err
          tvds-4.out
          tvds-4.out.err
          tvds-5.out
          tvds-5.out.err
          tvds_layout-1.out
          tvds_layout-1.out.err
          tvds_layout-2.out
          tvds_layout-2.out.err
          tvds_layout-3_1.out
          tvds_layout-3_1.out.err
          tvds_layout-3_2.out
          tvds_layout-3_2.out.err
          tvds_layout-4.out
          tvds_layout-4.out.err
          tvds_layout-5.out
          tvds_layout-5.out.err
    )
    set_tests_properties (H5LS_VDS-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5LS_VDS-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5LS_VDS-clearall-objects")
  endif ()

  ADD_H5_VDS_TEST (tvds-1 0 -w80 -v -S 1_vds.h5)
  ADD_H5_VDS_TEST (tvds-2 0 -w80 -v -S 2_vds.h5)
  ADD_H5_VDS_TEST (tvds-3_1 0 -w80 -v -S 3_1_vds.h5)
  ADD_H5_VDS_TEST (tvds-3_2 0 -w80 -v -S 3_2_vds.h5)
  ADD_H5_VDS_TEST (tvds-4 0 -w80 -v -S 4_vds.h5)
  ADD_H5_VDS_TEST (tvds-5 0 -w80 -v -S 5_vds.h5)

