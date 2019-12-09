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
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_FILES
      h5stat_err_refcount
      h5stat_err_old_layout
      h5stat_err_old_fill
      h5stat_help1
      h5stat_help2
      h5stat_notexist
      h5stat_nofile
      h5stat_filters
      h5stat_filters-file
      h5stat_filters-F
      h5stat_filters-d
      h5stat_filters-g
      h5stat_filters-dT
      h5stat_filters-UD
      h5stat_filters-UT
      h5stat_tsohm
      h5stat_newgrat
      h5stat_newgrat-UG
      h5stat_newgrat-UA
      h5stat_idx
      h5stat_links1
      h5stat_links2
      h5stat_links3
      h5stat_links4
      h5stat_links5
      h5stat_dims1
      h5stat_dims2
      h5stat_numattrs1
      h5stat_numattrs2
      h5stat_numattrs3
      h5stat_numattrs4
  )
  set (HDF5_REFERENCE_ERR_FILES
      h5stat_err_refcount
      h5stat_err_old_layout
      h5stat_err_old_fill
      h5stat_err1_dims
      h5stat_err1_links
      h5stat_err1_numattrs
      h5stat_err2_numattrs
      h5stat_notexist
      h5stat_nofile
  )
  set (HDF5_REFERENCE_TEST_FILES
      h5stat_err_refcount.h5
      h5stat_err_old_layout.h5
      h5stat_err_old_fill.h5
      h5stat_filters.h5
      h5stat_idx.h5
      h5stat_tsohm.h5
      h5stat_newgrat.h5
      h5stat_threshold.h5
  )

  foreach (ddl_file ${HDF5_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5STAT_SOURCE_DIR}/testfiles/${ddl_file}.ddl" "${PROJECT_BINARY_DIR}/${ddl_file}.ddl" "h5stat_files")
  endforeach ()

  foreach (h5_file ${HDF5_REFERENCE_ERR_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5STAT_SOURCE_DIR}/testfiles/${h5_file}.err" "${PROJECT_BINARY_DIR}/${h5_file}.err" "h5stat_files")
  endforeach ()

  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5STAT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/${h5_file}" "h5stat_files")
  endforeach ()
  add_custom_target(h5stat_files ALL COMMENT "Copying files needed by h5stat tests" DEPENDS ${h5stat_files_list})

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

  macro (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5STAT-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5stat${tgt_ext}> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5STAT-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5STAT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5stat${tgt_ext}>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_ERR_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5STAT-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5stat${tgt_ext}> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5STAT-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5STAT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5stat${tgt_ext}>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.mty"
              -D "TEST_ERRREF=${resultfile}.err"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    foreach (ddl_file ${HDF5_REFERENCE_FILES})
      set (CLEAR_LIST ${CLEAR_LIST} ${ddl_file}.out ${ddl_file}.out.err)
    endforeach ()
    add_test (
      NAME H5STAT-clearall-objects
      COMMAND ${CMAKE_COMMAND} -E remove ${CLEAR_LIST}
    )
  endif ()

# Test for help flag
  ADD_H5_TEST (h5stat_help1 0 -h)
  ADD_H5_TEST (h5stat_help2 0 --help)

# Test when h5stat a file that does not exist
  ADD_H5_TEST (h5stat_notexist 1 notexist.h5)
  ADD_H5_TEST (h5stat_nofile 1 '')

# Test file with groups, compressed datasets, user-applied fileters, etc.
# h5stat_filters.h5 is a copy of ../../testfiles/tfilters.h5 as of release 1.8.0-alpha4
  ADD_H5_TEST (h5stat_filters 0 h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-file 0 -f h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-F 0 -F h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-d 0 -d h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-g 0 -g h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-dT 0 -dT h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-UD 0 -D h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-UT 0 -T h5stat_filters.h5)
# h5stat_tsohm.h5 is a copy of ../../../test/tsohm.h5 generated by tsohm.c
# as of release 1.8.7-snap0 (on a 64-bit machine)
  ADD_H5_TEST (h5stat_tsohm 0 h5stat_tsohm.h5)
# h5stat_newgrat.h5 is generated by h5stat_gentest.c
  ADD_H5_TEST (h5stat_newgrat 0 h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UG 0 -G h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UA 0 -A h5stat_newgrat.h5)
# h5stat_idx.h5 is generated by h5stat_gentest.c
  ADD_H5_TEST (h5stat_idx 0 h5stat_idx.h5)
#
# Tests for -l (--links) option on h5stat_threshold.h5:
#   -l 0 (incorrect threshold value)
#   -g -l 8
#   --links=8
#   --links=20 -g
  ADD_H5_ERR_TEST (h5stat_err1_links 1 -l 0 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_links1 0 -g -l 8 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_links2 0 --links=8 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_links3 0 --links=20 -g h5stat_threshold.h5)
#
# Tests for -l (--links) option on h5stat_newgrat.h5:
#   -g
#   -g -l 40000
  ADD_H5_TEST (h5stat_links4 0 -g h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_links5 0 -g -l 40000 h5stat_newgrat.h5)
#
# Tests for -m (--dims) option on h5stat_threshold.h5
#   -d --dims=-1 (incorrect threshold value)
#   -gd -m 5
#   -d --di=15
  ADD_H5_ERR_TEST (h5stat_err1_dims 1 -d --dims=-1 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_dims1 0 -gd -m 5 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_dims2 0 -d --di=15 h5stat_threshold.h5)
#
# Tests for -a option on h5stat_threshold.h5
#   -a -2 (incorrect threshold value)
#   --numattrs (without threshold value)
#   -AS -a 10
#   -a 1
#   -A --numattrs=25
  ADD_H5_ERR_TEST (h5stat_err1_numattrs 1 -a -2 h5stat_threshold.h5)
  ADD_H5_ERR_TEST (h5stat_err2_numattrs 1 --numattrs h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_numattrs1 0 -AS -a 10 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_numattrs2 0 -a 1 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_numattrs3 0 -A --numattrs=25 h5stat_threshold.h5)
#
# Tests for -a option on h5stat_newgrat.h5
#   -A -a 100
  ADD_H5_TEST (h5stat_numattrs4 0 -A -a 100 h5stat_newgrat.h5)
#
# Tests to verify HDFFV-10333:
# h5stat_err_refcount.h5 is generated by h5stat_gentest.c
# h5stat_err_old_layout.h5 and h5stat_err_old_fill.h5: see explanation in h5stat_gentest.c
  ADD_H5_TEST (h5stat_err_refcount 1 h5stat_err_refcount.h5)
  ADD_H5_TEST (h5stat_err_old_layout 1 h5stat_err_old_layout.h5)
  ADD_H5_TEST (h5stat_err_old_fill 1 h5stat_err_old_fill.h5)
#
#
