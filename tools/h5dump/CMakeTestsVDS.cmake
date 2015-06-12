
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
  )
  set (HDF5_ERROR_REFERENCE_VDS
  )

  foreach (vds_h5_file ${HDF5_REFERENCE_TEST_VDS})
    GET_FILENAME_COMPONENT(fname "${vds_h5_file}" NAME)
    set (dest "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}")
    #message (STATUS " Copying ${vds_h5_file}")
    add_custom_command (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/vds/${vds_h5_file} ${dest}
    )
  endforeach (vds_h5_file ${HDF5_REFERENCE_TEST_VDS})


  foreach (ddl_vds ${HDF5_REFERENCE_VDS})
    GET_FILENAME_COMPONENT(fname "${ddl_vds}" NAME)
    set (ddldest "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}")
    #message (STATUS " Copying ${ddl_vds}")
    add_custom_command (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/vds/${ddl_vds} ${ddldest}
    )
  endforeach (ddl_vds ${HDF5_REFERENCE_VDS})

  foreach (ddl_vds ${HDF5_ERROR_REFERENCE_VDS})
    GET_FILENAME_COMPONENT(fname "${ddl_vds}" NAME)
    set (ddldest "${PROJECT_BINARY_DIR}/testfiles/vds/${fname}")
    #message (STATUS " Copying ${ddl_vds}")
    add_custom_command (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/errfiles/${ddl_vds} ${ddldest}
    )
  endforeach (ddl_vds ${HDF5_ERROR_REFERENCE_VDS})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_VDS_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfilesvds")
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif (NOT ${resultcode} STREQUAL "0")
      if (NOT "${last_vds_test}" STREQUAL "")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_VDS_test})
      endif (NOT "${last_vds_test}" STREQUAL "")
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
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
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_VDS_TEST file)

  MACRO (ADD_H5_VDS_LAYOUT resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> -p ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfilesvds")
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif (NOT ${resultcode} STREQUAL "0")
      if (NOT "${last_vds_test}" STREQUAL "")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_VDS_test})
      endif (NOT "${last_vds_test}" STREQUAL "")
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
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
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_VDS_LAYOUT file)

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
    set_tests_properties (H5DUMP_VDS-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
    if (NOT "${last_vds_test}" STREQUAL "")
      set_tests_properties (H5DUMP_VDS-clearall-objects PROPERTIES DEPENDS ${last_vds_test})
    endif (NOT "${last_vds_test}" STREQUAL "")
    set (last_VDS_test "H5DUMP_VDS-clearall-objects")
  endif (HDF5_ENABLE_USING_MEMCHECKER)

  # Data read
  ADD_H5_VDS_TEST (tvds-1 0 --enable-error-stack 1_vds.h5)
  ADD_H5_VDS_TEST (tvds-2 0 --enable-error-stack 2_vds.h5)
  ADD_H5_VDS_TEST (tvds-3_1 0 --enable-error-stack 3_1_vds.h5)
  ADD_H5_VDS_TEST (tvds-3_2 0 --enable-error-stack 3_2_vds.h5)
  ADD_H5_VDS_TEST (tvds-4 0 --enable-error-stack 4_vds.h5)
  ADD_H5_VDS_TEST (tvds-5 0 --enable-error-stack 5_vds.h5)

  # Layout read
  ADD_H5_VDS_LAYOUT (tvds_layout-1 0 --enable-error-stack 1_vds.h5)
  ADD_H5_VDS_LAYOUT (tvds_layout-2 0 --enable-error-stack 2_vds.h5)
  ADD_H5_VDS_LAYOUT (tvds_layout-3_1 0 --enable-error-stack 3_1_vds.h5)
  ADD_H5_VDS_LAYOUT (tvds_layout-3_2 0 --enable-error-stack 3_2_vds.h5)
  ADD_H5_VDS_LAYOUT (tvds_layout-4 0 --enable-error-stack 4_vds.h5)
  ADD_H5_VDS_LAYOUT (tvds_layout-5 0 --enable-error-stack 5_vds.h5)
