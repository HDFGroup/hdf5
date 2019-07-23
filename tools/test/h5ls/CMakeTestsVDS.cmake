#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

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

  foreach (listfiles ${LIST_HDF5_TEST_FILES})
    get_filename_component(fname "${listfiles}" NAME)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/vds/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/vds/prefix/${fname}" "h5ls_vds_files")
  endforeach ()

  foreach (listfiles ${LIST_OTHER_TEST_FILES})
    get_filename_component(fname "${listfiles}" NAME)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5LS_SOURCE_DIR}/vds_prefix/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/vds/prefix/${fname}" "h5ls_vds_files")
  endforeach ()

  add_custom_target(h5ls_vds_files ALL COMMENT "Copying files needed by h5ls_vds tests" DEPENDS ${h5ls_vds_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  if (NOT BUILD_SHARED_LIBS)
    set (tgt_ext "")
  else ()
    set (tgt_ext "-shared")
  endif ()

  macro (ADD_H5_VDS_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5LS-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5ls${tgt_ext}> ${ARGN})
      set_tests_properties (H5LS-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/vds")
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5LS-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME H5LS-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls${tgt_ext}>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/vds"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_VDS_PREFIX_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5LS_PREFIX-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5ls${tgt_ext}> ${ARGN})
      set_tests_properties (H5LS_PREFIX-${resultfile} PROPERTIES
          ENVIRONMENT "HDF5_VDS_PREFIX=\${ORIGIN}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
      )
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5LS_PREFIX-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME H5LS_PREFIX-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls${tgt_ext}>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=vds/prefix/${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=vds/prefix/${resultfile}.ls"
              -D "TEST_ENV_VAR=HDF5_VDS_PREFIX"
              -D "TEST_ENV_VALUE=\${ORIGIN}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  ADD_H5_VDS_TEST (tvds-1 0 -w80 -v -S 1_vds.h5)
  ADD_H5_VDS_TEST (tvds-2 0 -w80 -v -S 2_vds.h5)
  ADD_H5_VDS_TEST (tvds-3_1 0 -w80 -v -S 3_1_vds.h5)
  ADD_H5_VDS_TEST (tvds-3_2 0 -w80 -v -S 3_2_vds.h5)
  ADD_H5_VDS_TEST (tvds-4 0 -w80 -v -S 4_vds.h5)
  ADD_H5_VDS_TEST (tvds-5 0 -w80 -v -S 5_vds.h5)

  ADD_H5_VDS_PREFIX_TEST (tvds-1 0 -w80 -v -S vds/prefix/1_vds.h5)
  ADD_H5_VDS_PREFIX_TEST (tvds-2 0 -w80 -v -S vds/prefix/2_vds.h5)
  ADD_H5_VDS_PREFIX_TEST (tvds-3_1 0 -w80 -v -S vds/prefix/3_1_vds.h5)
  ADD_H5_VDS_PREFIX_TEST (tvds-3_2 0 -w80 -v -S vds/prefix/3_2_vds.h5)
  ADD_H5_VDS_PREFIX_TEST (tvds-4 0 -w80 -v -S vds/prefix/4_vds.h5)
  ADD_H5_VDS_PREFIX_TEST (tvds-5 0 -w80 -v -S vds/prefix/5_vds.h5)

