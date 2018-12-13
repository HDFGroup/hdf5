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

set (H5WATCH_TEST_FILES
    w-help1.ddl
    w-err-cmpd1.err
    w-err-cmpd2.err
    w-err-cmpd3.err
    w-err-cmpd4.err
    w-err-cmpd5.err
    w-err-dset1.err
    w-err-dset2.err
    w-err-dset-nomax.err
    w-err-dset-none.err
    w-err-file.err
    w-err-poll.ddl
    w-err-poll0.ddl
    w-err-width.ddl
    w-ext-cmpd.ddl
    w-ext-cmpd-esc.ddl
    w-ext-cmpd-esc-f1.ddl
    w-ext-cmpd-esc-f3.ddl
    w-ext-cmpd-esc-ff2.ddl
    w-ext-cmpd-f1.ddl
    w-ext-cmpd-f2.ddl
    w-ext-cmpd-ff3.ddl
    w-ext-cmpd-label.ddl
    w-ext-cmpd-two.ddl
    w-ext-cmpd-two-f1.ddl
    w-ext-cmpd-two-f3.ddl
    w-ext-cmpd-two-ff2.ddl
    w-ext-early.ddl
    w-ext-late.ddl
    w-ext-one.ddl
    w-ext-one-d.ddl
    w-ext-one-simple.ddl
    w-ext-two.ddl
    w-ext-two-d.ddl
    w-ext-two-width.ddl
)

# make test dir
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

foreach (h5watch_file ${H5WATCH_TEST_FILES})
  HDFTEST_COPY_FILE("${HDF5_HL_TOOLS_DIR}/testfiles/${h5watch_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5watch_file}" "H5WATCH_files")
endforeach ()
add_custom_target(H5WATCH_files ALL COMMENT "Copying files needed by H5WATCH tests" DEPENDS ${H5WATCH_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_H5_TEST resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5WATCH_ARGS-h5watch-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5watch>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5WATCH_ARGS-h5watch-${resultfile} PROPERTIES DEPENDS ${last_test})
      set (last_test "H5WATCH_ARGS-h5watch-${resultfile}")
    endif ()
  endmacro ()

  macro (ADD_H5_ERR_TEST resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5WATCH_ARGS-h5watch-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5watch>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.mty"
              -D "TEST_ERRREF=${resultfile}.err"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5WATCH_ARGS-h5watch-${resultfile} PROPERTIES DEPENDS ${last_test})
      set (last_test "H5WATCH_ARGS-h5watch-${resultfile}")
    endif ()
  endmacro ()

  macro (ADD_H5_WATCH resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5WATCH-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
                  ${resultfile}.h5
      )
      set_tests_properties (H5WATCH-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      add_test (
          NAME H5WATCH-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5watch>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.txt"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5WATCH-${resultfile} PROPERTIES DEPENDS H5WATCH-${resultfile}-clear-objects)
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Check to see if the VFD specified by the HDF5_DRIVER environment variable
# supports SWMR.
set (SWMR_INCOMPAT ${hl_swmr_check_compat_vfd})

if (NOT SWMR_INCOMPAT)
# Remove any output file left over from previous test run
  add_test (
    NAME H5WATCH-clearall-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        WATCH.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (H5WATCH-clearall-objects PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "H5WATCH-clearall-objects")

#################################################################################################
#                                               #
# WATCH.h5: file with various types of datasets for testing--                   #
#   The following datasets are chunked, H5D_ALLOC_TIME_INCR, max. dimensional setting:      #
#       DSET_ONE: one-dimensional dataset                           #
#       DSET_TWO: two-dimensional dataset                           #
#       DSET_CMPD: one-dimensional dataset with compound type                   #
#       DSET_CMPD_ESC: one-dimensional dataset with compound type & escape/separator characters #
#       DSET_CMPD_TWO: two-dimensional dataset with compound type               #
#                                               #
#   The following datasets are one-dimensional, chunked, max. dimension setting:        #
#       DSET_ALLOC_EARLY: dataset with H5D_ALLOC_TIME_EARLY                 #
#       DSET_ALLOC_LATE: dataset H5D_ALLOC_TIME_LATE                        #
#                                               #
#   The following datasets are one-dimensional:                         #
#   DSET_NONE: fixed dimension setting, contiguous, H5D_ALLOC_TIME_LATE         #
#   DSET_NOMAX: fixed dimension setting, chunked, H5D_ALLOC_TIME_INCR           #
#                                               #
#################################################################################################
# create the output files to be used.
  add_test (NAME H5WATCH-h5watchgentest COMMAND $<TARGET_FILE:h5watchgentest>)
  set_tests_properties (H5WATCH-h5watchgentest PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  set_tests_properties (H5WATCH-h5watchgentest PROPERTIES DEPENDS "H5WATCH-clearall-objects")
  set (last_test "H5WATCH-h5watchgentest")

# Test on --help options
  ADD_H5_TEST (w-help1 0 --help)
#
# Tests on expected failures
  ADD_H5_ERR_TEST (w-err-dset1 1 WATCH.h5)
  ADD_H5_ERR_TEST (w-err-dset2 1 WATCH.h5/group/DSET_CMPD)
  ADD_H5_ERR_TEST (w-err-dset-none 1 WATCH.h5/DSET_NONE)
  ADD_H5_ERR_TEST (w-err-dset-nomax 1 WATCH.h5/DSET_NOMAX)
  ADD_H5_ERR_TEST (w-err-file 1 ../WATCH.h5/DSET_CMPD)
  ADD_H5_TEST (w-err-width 1 --width=-8 WATCH.h5/DSET_ONE)
  ADD_H5_TEST (w-err-poll 1 --polling=-8 WATCH.h5/DSET_ONE)
  ADD_H5_TEST (w-err-poll0 1 --polling=0 WATCH.h5/DSET_ONE)
#
# Tests on invalid field names via --fields option for a compound typed dataset: DSET_CMPD
  ADD_H5_ERR_TEST (w-err-cmpd1 1 --fields=fieldx WATCH.h5/DSET_CMPD)
  ADD_H5_ERR_TEST (w-err-cmpd2 1 --fields=field1,field2. WATCH.h5/DSET_CMPD)
  ADD_H5_ERR_TEST (w-err-cmpd3 1 --fields=field1,field2, WATCH.h5/DSET_CMPD)
  ADD_H5_ERR_TEST (w-err-cmpd4 1 --fields=field1,field2.b.k WATCH.h5/DSET_CMPD)
  ADD_H5_ERR_TEST (w-err-cmpd5 1 --fields=field1 --fields=field2.b.k WATCH.h5/DSET_CMPD)
#
endif ()
