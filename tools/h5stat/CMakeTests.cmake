
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_FILES
      h5stat_help1.ddl
      h5stat_help2.ddl
      h5stat_notexist.ddl
      h5stat_nofile.ddl
      h5stat_filters.ddl
      h5stat_filters-file.ddl
      h5stat_filters-F.ddl
      h5stat_filters-d.ddl
      h5stat_filters-g.ddl
      h5stat_filters-dT.ddl
      h5stat_filters-UD.ddl
      h5stat_filters-UT.ddl
      h5stat_tsohm.ddl
      h5stat_newgrat.ddl
      h5stat_newgrat-UG.ddl
      h5stat_newgrat-UA.ddl
      h5stat_err1_links.ddl
      h5stat_links1.ddl
      h5stat_links2.ddl
      h5stat_links3.ddl
      h5stat_links4.ddl
      h5stat_links5.ddl
      h5stat_err1_dims.ddl
      h5stat_dims1.ddl
      h5stat_dims2.ddl
      h5stat_err1_numattrs.ddl
      h5stat_err2_numattrs.ddl
      h5stat_numattrs1.ddl
      h5stat_numattrs2.ddl
      h5stat_numattrs3.ddl
      h5stat_numattrs4.ddl
  )
  set (HDF5_REFERENCE_TEST_FILES
      h5stat_filters.h5
      h5stat_tsohm.h5
      h5stat_newgrat.h5
      h5stat_threshold.h5
  )

  foreach (ddl_file ${HDF5_REFERENCE_FILES})
    set (ddldest "${PROJECT_BINARY_DIR}/${ddl_file}")
    #message (STATUS " Translating ${ddl_file}")
    add_custom_command (
        TARGET     h5stat
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/testfiles/${ddl_file} ${ddldest}
    )
  endforeach (ddl_file ${HDF5_REFERENCE_FILES})

  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    set (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #message (STATUS " Copying ${h5_file}")
    add_custom_command (
        TARGET     h5stat
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  endforeach (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5STAT-${resultfile} COMMAND $<TARGET_FILE:h5stat> ${ARGN})
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5STAT-${resultfile} PROPERTIES WILL_FAIL "true")
      endif (NOT ${resultcode} STREQUAL "0")
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (H5STAT-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif (NOT "${last_test}" STREQUAL "")
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5STAT-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      add_test (
          NAME H5STAT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5STAT-${resultfile} PROPERTIES DEPENDS "H5STAT-${resultfile}-clear-objects")
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
      NAME H5STAT-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          h5stat_help1.out
          h5stat_help1.out.err
          h5stat_help2.out
          h5stat_help2.out.err
          h5stat_notexist.out
          h5stat_notexist.out.err
          h5stat_nofile.out
          h5stat_nofile.out.err
          h5stat_filters.out
          h5stat_filters.out.err
          h5stat_filters-file.out
          h5stat_filters-file.out.err
          h5stat_filters-F.out
          h5stat_filters-F.out.err
          h5stat_filters-d.out
          h5stat_filters-d.out.err
          h5stat_filters-g.out
          h5stat_filters-g.out.err
          h5stat_filters-dT.out
          h5stat_filters-dT.out.err
          h5stat_filters-UD.out
          h5stat_filters-UD.out.err
          h5stat_filters-UT.out
          h5stat_filters-UT.out.err
          h5stat_tsohm.out
          h5stat_tsohm.out.err
          h5stat_newgrat.out
          h5stat_newgrat.out.err
          h5stat_newgrat-UG.out
          h5stat_newgrat-UG.out.err
          h5stat_newgrat-UA.out
          h5stat_newgrat-UA.out.err
          h5stat_err1_links.out
          h5stat_err1_links.out.err
          h5stat_links1.out
          h5stat_links1.out.err
          h5stat_links2.out
          h5stat_links2.out.err
          h5stat_links3.out
          h5stat_links3.out.err
          h5stat_links4.out
          h5stat_links4.out.err
          h5stat_links5.out
          h5stat_links5.out.err
          h5stat_err1_dims.out
          h5stat_err1_dims.out.err
          h5stat_dims1.out
          h5stat_dims1.out.err
          h5stat_dims2.out
          h5stat_dims2.out.err
          h5stat_err1_numattrs.out
          h5stat_err1_numattrs.out.err
          h5stat_err2_numattrs.out
          h5stat_err2_numattrs.out.err
          h5stat_numattrs1.out
          h5stat_numattrs1.out.err
          h5stat_numattrs2.out
          h5stat_numattrs2.out.err
          h5stat_numattrs3.out
          h5stat_numattrs3.out.err
          h5stat_numattrs4.out
          h5stat_numattrs4.out.err
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5STAT-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif (NOT "${last_test}" STREQUAL "")
    set (last_test "H5STAT-clearall-objects")
  endif (HDF5_ENABLE_USING_MEMCHECKER)

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
# as of release 1.8.0-alpha4
  ADD_H5_TEST (h5stat_tsohm 0 h5stat_tsohm.h5)
# h5stat_newgrat.h5 is generated by h5stat_gentest.c
  ADD_H5_TEST (h5stat_newgrat 0 h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UG 0 -G h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UA 0 -A h5stat_newgrat.h5)
#
# Tests for -l (--links) option on h5stat_threshold.h5: 
#   -l 0 (incorrect threshold value)
#   -g -l 8
#   --links=8
#   --links=20 -g
  ADD_H5_TEST (h5stat_err1_links 1 -l 0 h5stat_threshold.h5)
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
  ADD_H5_TEST (h5stat_err1_dims 1 -d --dims=-1 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_dims1 0 -gd -m 5 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_dims2 0 -d --di=15 h5stat_threshold.h5)
#
# Tests for -a option on h5stat_threshold.h5
#   -a -2 (incorrect threshold value)
#   --numattrs (without threshold value)
#   -AS -a 10
#   -a 1
#   -A --numattrs=25
  ADD_H5_TEST (h5stat_err1_numattrs 1 -a -2 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_err2_numattrs 1 --numattrs h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_numattrs1 0 -AS -a 10 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_numattrs2 0 -a 1 h5stat_threshold.h5)
  ADD_H5_TEST (h5stat_numattrs3 0 -A --numattrs=25 h5stat_threshold.h5)
#
# Tests for -a option on h5stat_newgrat.h5
#   -A -a 100
  ADD_H5_TEST (h5stat_numattrs4 0 -A -a 100 h5stat_newgrat.h5)
#
  