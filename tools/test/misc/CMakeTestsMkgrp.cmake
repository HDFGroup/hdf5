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
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  set (HDF5_MKGRP_TEST_FILES
      #h5mkgrp_help.txt
      #h5mkgrp_version
      h5mkgrp_single.ls
      h5mkgrp_single_v.ls
      h5mkgrp_single_p.ls
      h5mkgrp_single_l.ls
      h5mkgrp_several.ls
      h5mkgrp_several_v.ls
      h5mkgrp_several_p.ls
      h5mkgrp_several_l.ls
      h5mkgrp_nested_p.ls
      h5mkgrp_nested_lp.ls
      h5mkgrp_nested_mult_p.ls
      h5mkgrp_nested_mult_lp.ls
  )

  # make test dir
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  foreach (h5_mkgrp_file ${HDF5_MKGRP_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_mkgrp_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_mkgrp_file}" "h5mkgrp_files")
  endforeach ()

  HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/testfiles/h5mkgrp_help.txt" "${PROJECT_BINARY_DIR}/testfiles/h5mkgrp_help.txt" "h5mkgrp_files")
  add_custom_target(h5mkgrp_files ALL COMMENT "Copying files needed by h5mkgrp tests" DEPENDS ${h5mkgrp_files_list})

  configure_file (${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/testfiles/h5mkgrp_version.txt.in ${PROJECT_BINARY_DIR}/testfiles/h5mkgrp_version.txt @ONLY)

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

  macro (ADD_H5_TEST resultfile resultcode resultoption)
    add_test (
        NAME H5MKGRP-${resultfile}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}.h5
    )
    set_tests_properties (H5MKGRP-${resultfile}-clear-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    add_test (
        NAME H5MKGRP-${resultfile}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5mkgrp${tgt_ext}> ${resultoption} ${resultfile}.h5 ${ARGN}
    )
    set_tests_properties (H5MKGRP-${resultfile} PROPERTIES
        DEPENDS H5MKGRP-${resultfile}-clear-objects
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5MKGRP-${resultfile}-h5ls
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls${tgt_ext}>"
              -D "TEST_ARGS:STRING=-v;-r;${resultfile}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_MASK_MOD=true"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5MKGRP-${resultfile}-h5ls PROPERTIES DEPENDS H5MKGRP-${resultfile})
    endif ()
  endmacro ()

  macro (ADD_H5_CMP resultfile resultcode)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5MKGRP_CMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5mkgrp> ${ARGN})
    else ()
      add_test (
          NAME H5MKGRP_CMP-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${resultfile}.h5
      )
      set_tests_properties (H5MKGRP_CMP-${resultfile}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
      )
      add_test (
          NAME H5MKGRP_CMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5mkgrp${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.txt"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5MKGRP_CMP-${resultfile} PROPERTIES
          DEPENDS H5MKGRP_CMP-${resultfile}-clear-objects
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5MKGRP-clearall-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            h5mkgrp_single.h5
            h5mkgrp_single_v.h5
            h5mkgrp_single_p.h5
            h5mkgrp_single_l.h5
            h5mkgrp_several.h5
            h5mkgrp_several_v.h5
            h5mkgrp_several_p.h5
            h5mkgrp_several_l.h5
            h5mkgrp_nested_p.h5
            h5mkgrp_nested_lp.h5
            h5mkgrp_nested_mult_p.h5
            h5mkgrp_nested_mult_lp.h5
    )
    set_tests_properties (H5MKGRP-clearall-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
  endif ()

  # Check that help & version is displayed properly
  ADD_H5_CMP (h5mkgrp_help 0 "-h")
  ADD_H5_CMP (h5mkgrp_version 0 "-V")

  # Create single group at root level
  ADD_H5_TEST (h5mkgrp_single 0 "" single)
  ADD_H5_TEST (h5mkgrp_single_v 0 "-v" single)
  ADD_H5_TEST (h5mkgrp_single_p 0 "-p" single)
  ADD_H5_TEST (h5mkgrp_single_l 0 "-l" latest)

  # Create several groups at root level
  ADD_H5_TEST (h5mkgrp_several 0 "" one two)
  ADD_H5_TEST (h5mkgrp_several_v 0 "-v" one two)
  ADD_H5_TEST (h5mkgrp_several_p 0 "-p" one two)
  ADD_H5_TEST (h5mkgrp_several_l 0 "-l" one two)

  # Create various nested groups
  ADD_H5_TEST (h5mkgrp_nested_p 0 "-p" /one/two)
  ADD_H5_TEST (h5mkgrp_nested_lp 0 "-lp" /one/two)
  ADD_H5_TEST (h5mkgrp_nested_mult_p 0 "-p" /one/two /three/four)
  ADD_H5_TEST (h5mkgrp_nested_mult_lp 0 "-lp" /one/two /three/four)
