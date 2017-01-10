
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # VDS
  # --------------------------------------------------------------------
  #-- Copy all the HDF5 files from the test directory into the source directory
  set (HDF5_REFERENCE_VDS
      tvds-1.ddl
      tvds-2.ddl
      tvds-3_1.ddl
      tvds-3_2.ddl
      tvds-4.ddl
      tvds-5.ddl
      tvds_layout-1.ddl
      tvds_layout-2.ddl
      tvds_layout-3_1.ddl
      tvds_layout-3_2.ddl
      tvds_layout-4.ddl
      tvds_layout-5.ddl
      vds-first.ddl
      vds-gap1.ddl
      vds-gap2.ddl
      vds_layout-eiger.ddl
      vds_layout-maxmin.ddl
  )
  set (HDF5_REFERENCE_TEST_VDS
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
      a.h5
      b.h5
      c.h5
      d.h5
      vds-percival-unlim-maxmin.h5
      f-0.h5
      f-3.h5
      vds-eiger.h5
  )
  set (HDF5_ERROR_REFERENCE_VDS
  )

  foreach (vds_h5_file ${HDF5_REFERENCE_TEST_VDS})
    get_filename_component(fname "${vds_h5_file}" NAME)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/vds/${vds_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}" "h5dump_vds_files")
  endforeach ()


  foreach (ddl_vds ${HDF5_REFERENCE_VDS})
    get_filename_component(fname "${ddl_vds}" NAME)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/vds/${ddl_vds}" "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}" "h5dump_vds_files")
  endforeach ()

  foreach (ddl_vds ${HDF5_ERROR_REFERENCE_VDS})
    get_filename_component(fname "${ddl_vds}" NAME)
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/errfiles/${ddl_vds}" "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}" "h5dump_vds_files")
  endforeach ()
  add_custom_target(h5dump_vds_files ALL COMMENT "Copying files needed by h5dump_vds tests" DEPENDS ${h5dump_vds_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_VDS_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (NOT "${last_vds_test}" STREQUAL "")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_VDS_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/vds"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  ENDMACRO ()

  MACRO (ADD_H5_VDS_LAYOUT resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> -p ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (NOT "${last_vds_test}" STREQUAL "")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_VDS_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=-p;${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/vds"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
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
      NAME H5DUMP_VDS-clearall-objects
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
          vds-first.out
          vds-first.out.err
          vds-gap1.out
          vds-gap1.out.err
          vds-gap2.out
          vds-gap2.out.err
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
          vds_layout-eiger.out
          vds_layout-eiger.out.err
          vds_layout-maxmin.out
          vds_layout-maxmin.out.err
    )
    set_tests_properties (H5DUMP_VDS-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
    if (NOT "${last_vds_test}" STREQUAL "")
      set_tests_properties (H5DUMP_VDS-clearall-objects PROPERTIES DEPENDS ${last_vds_test})
    endif ()
    set (last_VDS_test "H5DUMP_VDS-clearall-objects")
  endif ()

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
  if (H5_HAVE_FILTER_DEFLATE)
    set (USE_FILTER_DEFLATE "true")
  endif ()

  if (H5_HAVE_FILTER_SZIP)
    set (USE_FILTER_SZIP "true")
  endif ()

  # Data read
  if (USE_FILTER_DEFLATE)
    ADD_H5_VDS_TEST (tvds-1 0 --enable-error-stack 1_vds.h5)
    ADD_H5_VDS_TEST (tvds-2 0 --enable-error-stack 2_vds.h5)
    ADD_H5_VDS_TEST (tvds-3_1 0 --enable-error-stack 3_1_vds.h5)
    ADD_H5_VDS_TEST (tvds-3_2 0 --enable-error-stack 3_2_vds.h5)
    ADD_H5_VDS_TEST (tvds-4 0 --enable-error-stack 4_vds.h5)
    ADD_H5_VDS_TEST (tvds-5 0 --enable-error-stack 5_vds.h5)
    ADD_H5_VDS_TEST (vds-first 0 --vds-view-first-missing --enable-error-stack vds-percival-unlim-maxmin.h5)
    ADD_H5_VDS_TEST (vds-gap1 0 -d /VDS-Eiger --vds-gap-size=1 --enable-error-stack vds-eiger.h5)
    ADD_H5_VDS_TEST (vds-gap2 0 --vds-gap-size=2 --enable-error-stack vds-eiger.h5)
  endif ()

  # Layout read
  if (USE_FILTER_DEFLATE)
    ADD_H5_VDS_LAYOUT (tvds_layout-1 0 --enable-error-stack 1_vds.h5)
    ADD_H5_VDS_LAYOUT (tvds_layout-2 0 --enable-error-stack 2_vds.h5)
    ADD_H5_VDS_LAYOUT (tvds_layout-3_1 0 --enable-error-stack 3_1_vds.h5)
    ADD_H5_VDS_LAYOUT (tvds_layout-3_2 0 --enable-error-stack 3_2_vds.h5)
    ADD_H5_VDS_LAYOUT (tvds_layout-4 0 --enable-error-stack 4_vds.h5)
    ADD_H5_VDS_LAYOUT (tvds_layout-5 0 --enable-error-stack 5_vds.h5)
    ADD_H5_VDS_LAYOUT (vds_layout-eiger 0 --enable-error-stack vds-eiger.h5)
    ADD_H5_VDS_LAYOUT (vds_layout-maxmin 0 --enable-error-stack vds-percival-unlim-maxmin.h5)
  endif ()
