#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
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
      ${HDF5_TOOLS_DIR}/testfiles/tgrpnullspace.h5
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

  set (LIST_ERR_TEST_FILES
      ${HDF5_TOOLS_DIR}/test/h5ls/errfiles/nosuchfile.err
      ${HDF5_TOOLS_DIR}/test/h5ls/errfiles/textlinksrc-nodangle-1.err
      ${HDF5_TOOLS_DIR}/test/h5ls/errfiles/tgroup-1.err
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
      ${HDF5_TOOLS_DIR}/testfiles/tgrpnullspace.ls
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
    HDFTEST_COPY_FILE("${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${fname}" "dh5walk_files")
  endforeach ()
  foreach (listfiles ${LIST_ERR_TEST_FILES})
    get_filename_component(fname "${listfiles}" NAME)
    HDFTEST_COPY_FILE("${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${fname}" "dh5walk_files")
  endforeach ()
  add_custom_target(dh5walk_files ALL COMMENT "Copying files needed by h5ls tests" DEPENDS ${dh5walk_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME DH5WALK-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:dh5walk${tgt_file_ext}> ${ARGN})
      set_tests_properties (DH5WALK-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (DH5WALK-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      # Remove any output file left over from previous test run
      add_test (
          NAME DH5WALK-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:dh5walk${tgt_file_ext}>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.dh5walk"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  ADD_H5_TEST(help-1 0 -h)
