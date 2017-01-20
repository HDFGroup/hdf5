
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # Copy all the test files from source directory to test directory
  # --------------------------------------------------------------------
  set (LIST_HDF5_TEST_FILES
      ${HDF5_TOOLS_DIR}/testfiles/tall.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattr2.h5
      ${HDF5_TOOLS_DIR}/testfiles/tattrreg.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdatareg.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdset.h5
      ${HDF5_TOOLS_DIR}/testfiles/tempty.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc.h5
      ${HDF5_TOOLS_DIR}/testfiles/textlinktar.h5
      ${HDF5_TOOLS_DIR}/testfiles/tgroup.h5
      ${HDF5_TOOLS_DIR}/testfiles/tgrp_comments.h5
      ${HDF5_TOOLS_DIR}/testfiles/thlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tloop.h5
      ${HDF5_TOOLS_DIR}/testfiles/tnestedcomp.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsaf.h5
      ${HDF5_TOOLS_DIR}/testfiles/tslink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks.h5
      ${HDF5_TOOLS_DIR}/testfiles/tstr.h5
      ${HDF5_TOOLS_DIR}/testfiles/tudfilter.h5
      ${HDF5_TOOLS_DIR}/testfiles/tudlink.h5
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tdset_idx.h5
  )

  set (LIST_OTHER_TEST_FILES
      ${HDF5_TOOLS_DIR}/testfiles/help-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/help-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/help-3.ls
      ${HDF5_TOOLS_DIR}/testfiles/nosuchfile.ls
      ${HDF5_TOOLS_DIR}/testfiles/tall-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tall-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tattr2.ls
      ${HDF5_TOOLS_DIR}/testfiles/tattrreg_le.ls
      ${HDF5_TOOLS_DIR}/testfiles/tattrreg_be.ls
      ${HDF5_TOOLS_DIR}/testfiles/tcomp-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tdataregbe.ls
      ${HDF5_TOOLS_DIR}/testfiles/tdataregle.ls
      ${HDF5_TOOLS_DIR}/testfiles/tdset-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tempty.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlink-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-3.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-4.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-5.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-6.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-7.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-1-old.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-2-old.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-3-old.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-6-old.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-7-old.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks-3.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks-4.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks-5.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-nodangle-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/textlinksrc-nodangle-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/tgrp_comments.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsoftlinks-nodangle-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/thlinks-nodangle-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tgroup.ls
      ${HDF5_TOOLS_DIR}/testfiles/tgroup-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tgroup-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/tgroup-3.ls
      ${HDF5_TOOLS_DIR}/testfiles/thlink-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tloop-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tmultifile.ls
      ${HDF5_TOOLS_DIR}/testfiles/tnestcomp-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tnestcomp-2.ls
      ${HDF5_TOOLS_DIR}/testfiles/tnestcomp-3.ls
      ${HDF5_TOOLS_DIR}/testfiles/tnestcomp-4.ls
      ${HDF5_TOOLS_DIR}/testfiles/tsaf.ls
      ${HDF5_TOOLS_DIR}/testfiles/tslink-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tstr-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tudfilter.ls
      ${HDF5_TOOLS_DIR}/testfiles/tudlink-1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes1.ls
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes2le.ls
      ${HDF5_TOOLS_DIR}/testfiles/tvldtypes2be.ls
      ${HDF5_TOOLS_DIR}/testfiles/tdset_idx.ls
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  # copy the list of test files
  foreach (listfiles ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
    get_filename_component(fname "${listfiles}" NAME)
    HDFTEST_COPY_FILE("${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${fname}" "h5ls_files")
  endforeach ()
  add_custom_target(h5ls_files ALL COMMENT "Copying files needed by h5ls tests" DEPENDS ${h5ls_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5LS-${resultfile} COMMAND $<TARGET_FILE:h5ls> ${ARGN})
      set_tests_properties (H5LS-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
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
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  ENDMACRO ()

  MACRO (ADD_H5_UD_TEST testname resultcode resultfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5LS_UD-${testname}-clearall-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              testfiles/${resultfile}.out
              testfiles/${resultfile}.out.err
      )
      add_test (
          NAME H5LS_UD-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5LS_UD-${testname} PROPERTIES DEPENDS H5LS_UD-${testname}-clearall-objects)
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
      NAME H5LS-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          help-1.out
          help-1.out.err
          help-2.out
          help-2.out.err
          help-3.out
          help-3.out.err
          nosuchfile.out
          nosuchfile.out.err
          tall-1.out
          tall-1.out.err
          tall-2.out
          tall-2.out.err
          tarray1.out
          tarray1.out.err
          tattr2.out
          tattr2.out.err
          tcomp-1.out
          tcomp-1.out.err
          tdataregbe.out
          tdataregbe.out.err
          tdataregle.out
          tdataregle.out.err
          tdset-1.out
          tdset-1.out.err
          tempty.out
          tempty.out.err
          textlink-1.out
          textlink-1.out.err
          textlinksrc-1.out
          textlinksrc-1.out.err
          textlinksrc-2.out
          textlinksrc-2.out.err
          textlinksrc-3.out
          textlinksrc-3.out.err
          textlinksrc-4.out
          textlinksrc-4.out.err
          textlinksrc-5.out
          textlinksrc-5.out.err
          textlinksrc-6.out
          textlinksrc-6.out.err
          textlinksrc-7.out
          textlinksrc-7.out.err
          textlinksrc-1-old.out
          textlinksrc-1-old.out.err
          textlinksrc-2-old.out
          textlinksrc-2-old.out.err
          textlinksrc-3-old.out
          textlinksrc-3-old.out.err
          textlinksrc-6-old.out
          textlinksrc-6-old.out.err
          textlinksrc-7-old.out
          textlinksrc-7-old.out.err
          tgrp_comments.out
          tgrp_comments.out.err
          tsoftlinks-1.out
          tsoftlinks-1.out.err
          tsoftlinks-2.out
          tsoftlinks-2.out.err
          tsoftlinks-3.out
          tsoftlinks-3.out.err
          tsoftlinks-4.out
          tsoftlinks-4.out.err
          tsoftlinks-5.out
          tsoftlinks-5.out.err
          textlinksrc-nodangle-1.out
          textlinksrc-nodangle-1.out.err
          textlinksrc-nodangle-2.out
          textlinksrc-nodangle-2.out.err
          tsoftlinks-nodangle-1.out
          tsoftlinks-nodangle-1.out.err
          thlinks-nodangle-1.out
          thlinks-nodangle-1.out.err
          tgroup.out
          tgroup.out.err
          tgroup-1.out
          tgroup-1.out.err
          tgroup-2.out
          tgroup-2.out.err
          tgroup-3.out
          tgroup-3.out.err
          thlink-1.out
          thlink-1.out.err
          tloop-1.out
          tloop-1.out.err
          tnestcomp-1.out
          tnestcomp-1.out.err
          tnestcomp-2.out
          tnestcomp-2.out.err
          tnestcomp-3.out
          tnestcomp-3.out.err
          tnestcomp-4.out
          tnestcomp-4.out.err
          tsaf.out
          tsaf.out.err
          tslink-1.out
          tslink-1.out.err
          tstr-1.out
          tstr-1.out.err
          tudlink-1.out
          tudlink-1.out.err
          tvldtypes1.out
          tvldtypes1.out.err
          tvldtypes2le.out
          tvldtypes2le.out.err
          tvldtypes2be.out
          tvldtypes2be.out.err
    )
    set_tests_properties (H5LS-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5LS-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5LS-clearall-objects")
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

  # test the help syntax
  ADD_H5_TEST (help-1 0 -w80 -h)
  ADD_H5_TEST (help-2 0 -w80 --help)
  ADD_H5_TEST (help-3 0 -w80 -?)

  # test simple command
  ADD_H5_TEST (tall-1 0 -w80 tall.h5)
  ADD_H5_TEST (tall-2 0 -w80 -r -d tall.h5)
  ADD_H5_TEST (tgroup 0 -w80 tgroup.h5)
  ADD_H5_TEST (tgroup-3 0 -w80 tgroup.h5/g1)

  # test for displaying groups
  # The following combination of arguments is expected to return an error message
  # and return value 1
  ADD_H5_TEST (tgroup-1 1 -w80 -r -g tgroup.h5)
  ADD_H5_TEST (tgroup-2 0 -w80 -g tgroup.h5/g1)

  # test for files with groups that have long comments
  ADD_H5_TEST (tgrp_comments 0 -w80 -v -g tgrp_comments.h5/glongcomment)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 -w80 -r -d tdset.h5)

  # test for displaying soft links (dangle)
  ADD_H5_TEST (tslink-1 0 -w80 -r tslink.h5)

  # test for displaying more soft links with --follow-symlinks
  ADD_H5_TEST (tsoftlinks-1 0 --follow-symlinks tsoftlinks.h5)
  ADD_H5_TEST (tsoftlinks-2 0 --follow-symlinks -r tsoftlinks.h5)
  ADD_H5_TEST (tsoftlinks-3 0 --follow-symlinks tsoftlinks.h5/group1)
  ADD_H5_TEST (tsoftlinks-4 0 --follow-symlinks -r tsoftlinks.h5/group1)
  ADD_H5_TEST (tsoftlinks-5 0 --follow-symlinks tsoftlinks.h5/soft_dset1)

  # test for displaying external and user-defined links with --follow-symlinks
  ADD_H5_TEST (textlink-1 0 -w80 -r textlink.h5)
  ADD_H5_TEST (textlinksrc-1 0 -w80 --follow-symlinks -r textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-2 0 -w80 --follow-symlinks -rv textlinksrc.h5/ext_link5)
  ADD_H5_TEST (textlinksrc-3 0 -w80 --follow-symlinks -r textlinksrc.h5/ext_link1)
  ADD_H5_TEST (textlinksrc-4 0 -w80 -r textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-5 0 -w80 -r textlinksrc.h5/ext_link1)
  ADD_H5_TEST (textlinksrc-6 0 -w80 --follow-symlinks textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-7 0 -w80 --follow-symlinks textlinksrc.h5/ext_link1)
  ADD_H5_TEST (tudlink-1 0 -w80 -r tudlink.h5)

  # test for displaying external links with -E
  # the option -E will be depriciated but keep it for backward compatibility
  ADD_H5_TEST (textlinksrc-1-old 0 -w80 -Er textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-2-old 0 -w80 -Erv textlinksrc.h5/ext_link5)
  ADD_H5_TEST (textlinksrc-3-old 0 -w80 -Er textlinksrc.h5/ext_link1)
  ADD_H5_TEST (textlinksrc-6-old 0 -w80 -E textlinksrc.h5)
  ADD_H5_TEST (textlinksrc-7-old 0 -w80 -E textlinksrc.h5/ext_link1)

  # tests for no-dangling-links
  # if this option is given on dangling link, h5ls should return exit code 1
  # when used alone , expect to print out help and return exit code 1
  ADD_H5_TEST (textlinksrc-nodangle-1 1 -w80 --no-dangling-links textlinksrc.h5)
  # external dangling link - expected exit code 1
  ADD_H5_TEST (textlinksrc-nodangle-2 1 -w80 --follow-symlinks --no-dangling-links textlinksrc.h5)
  # soft dangling link - expected exit code 1
  ADD_H5_TEST (tsoftlinks-nodangle-1 1 -w80 --follow-symlinks --no-dangling-links tsoftlinks.h5)
  # when used file with no dangling links - expected exit code 0
  ADD_H5_TEST (thlinks-nodangle-1 0 -w80 --follow-symlinks --no-dangling-links thlink.h5)

# test for wildcards in filename (does not work with cmake)
#  ADD_H5_TEST (tstarfile 0 -w80 t*link.h5)
#  ADD_H5_TEST (tqmarkfile 0 -w80 t?link.h5)
  ADD_H5_TEST (tmultifile 0 -w80 thlink.h5 tslink.h5)

  # tests for hard links
  ADD_H5_TEST (thlink-1 0 -w80 thlink.h5)

  # tests for compound data types
  ADD_H5_TEST (tcomp-1 0 -w80 -r -d tcompound.h5)

  #test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 -w80 -r -d tnestedcomp.h5)

  ADD_H5_TEST (tnestcomp-2 0 -w80 -r -d -S tnestedcomp.h5)

  ADD_H5_TEST (tnestcomp-3 0 -w80 -r -d -l tnestedcomp.h5)

  ADD_H5_TEST (tnestcomp-4 0 -w80 -r -d -l -S tnestedcomp.h5)

  # test for loop detection
  ADD_H5_TEST (tloop-1 0 -w80 -r -d tloop.h5)

  # test for string
  ADD_H5_TEST (tstr-1 0 -w80 -r -d tstr.h5)

  # test test file created from lib SAF team
  ADD_H5_TEST (tsaf 0 -w80 -r -d tsaf.h5)

  # test for variable length data types
  ADD_H5_TEST (tvldtypes1 0 -w80 -r -d tvldtypes1.h5)

  # test for array data types
  ADD_H5_TEST (tarray1 0 -w80 -r -d tarray1.h5)

  # test for empty data
  ADD_H5_TEST (tempty 0 -w80 -d tempty.h5)

  # test for all dataset types written to attributes
  # enable -S for avoiding printing NATIVE types
  ADD_H5_TEST (tattr2 0 -w80 -v -S tattr2.h5)

  # test for attribute with region references wo verbose mode
  # ( HDFFV-7838, )
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tattrreg_be 0 -w80 -v -d tattrreg.h5)
  else (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tattrreg_le 0 -w80 -v -d tattrreg.h5)
  endif (H5_WORDS_BIGENDIAN)

  # test for non-existing file
  ADD_H5_TEST (nosuchfile 1 nosuchfile.h5)

  # test for variable length data types in verbose mode
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tvldtypes2be 0 -v tvldtypes1.h5)
  else (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tvldtypes2le 0 -v tvldtypes1.h5)
  endif (H5_WORDS_BIGENDIAN)

  # test for dataset region references data types in verbose mode
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tdataregbe 0 -v tdatareg.h5)
  else (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tdataregle 0 -v tdatareg.h5)
  endif (H5_WORDS_BIGENDIAN)

# test for file with datasets that use Fixed Array chunk indices
  if (USE_FILTER_DEFLATE)
    # data read internal filters
    ADD_H5_TEST (tdset_idx 0 -w80 -d tdset_idx.h5)
  endif (USE_FILTER_DEFLATE)


##############################################################################
###    P L U G I N  T E S T S
##############################################################################
ADD_H5_UD_TEST (h5ls_plugin_test 0 tudfilter -w80 -d tudfilter.h5)
