
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_TEST_FILES
      family_file00000.h5
      family_file00001.h5
      family_file00002.h5
      family_file00003.h5
      family_file00004.h5
      family_file00005.h5
      family_file00006.h5
      family_file00007.h5
      family_file00008.h5
      family_file00009.h5
      family_file00010.h5
      family_file00011.h5
      family_file00012.h5
      family_file00013.h5
      family_file00014.h5
      family_file00015.h5
      family_file00016.h5
      family_file00017.h5
  )

  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    set (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #message (STATUS " Copying ${h5_file}")
    add_custom_command (
        TARGET     h5repart
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_file} ${dest}
    )
  endforeach (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
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
    set (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_mkgrp_file}")
    #message (STATUS " Copying ${h5_mkgrp_file}")
    add_custom_command (
        TARGET     h5mkgrp
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_mkgrp_file} ${dest}
    )
  endforeach (h5_mkgrp_file ${HDF5_MKGRP_TEST_FILES})

  add_custom_command (
      TARGET     h5mkgrp
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/testfiles/h5mkgrp_help.txt ${PROJECT_BINARY_DIR}/testfiles/h5mkgrp_help.txt
  )
  configure_file (${PROJECT_SOURCE_DIR}/testfiles/h5mkgrp_version.txt.in ${PROJECT_BINARY_DIR}/testfiles/h5mkgrp_version.txt @ONLY)

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode resultoption)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5MKGRP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
                  ${resultfile}.h5 
                  ${resultfile}.out
                  ${resultfile}.out.err
      )
      set_tests_properties (H5MKGRP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    endif (NOT HDF5_ENABLE_USING_MEMCHECKER)
      
    add_test (
        NAME H5MKGRP-${resultfile}
        COMMAND $<TARGET_FILE:h5mkgrp> ${resultoption} ${resultfile}.h5 ${ARGN}
    )
    set_tests_properties (H5MKGRP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    if (HDF5_ENABLE_USING_MEMCHECKER)
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (H5MKGRP-${resultfile} PROPERTIES DEPENDS ${last_test})
      endif (NOT "${last_test}" STREQUAL "")
    else (HDF5_ENABLE_USING_MEMCHECKER)
      set_tests_properties (H5MKGRP-${resultfile} PROPERTIES DEPENDS H5MKGRP-${resultfile}-clear-objects)
      add_test (
          NAME H5MKGRP-${resultfile}-h5ls
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
              -D "TEST_ARGS:STRING=-v;-r;${resultfile}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_MASK_MOD=true"
              -D "TEST_REFERENCE=${resultfile}.ls"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5MKGRP-${resultfile}-h5ls PROPERTIES DEPENDS H5MKGRP-${resultfile})
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST resultfile resultcode resultoption)

  MACRO (ADD_H5_CMP resultfile resultcode)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5MKGRP_CMP-${resultfile} COMMAND $<TARGET_FILE:h5mkgrp> ${ARGN})
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5MKGRP_CMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
                  ${resultfile}.h5 
                  ${resultfile}.out
                  ${resultfile}.out.err
      )
      set_tests_properties (H5MKGRP_CMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      add_test (
          NAME H5MKGRP_CMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5mkgrp>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.txt"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5MKGRP_CMP-${resultfile} PROPERTIES DEPENDS H5MKGRP_CMP-${resultfile}-clear-objects)
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_CMP resultfile resultcode)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  add_test (
    NAME H5REPART-clearall-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove 
        fst_family00000.h5
        scd_family00000.h5
        scd_family00001.h5
        scd_family00002.h5
        scd_family00003.h5
        family_to_sec2.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (H5REPART-clearall-objects PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")
  set (last_test "H5REPART-clearall-objects")
  
  # repartition family member size to 20,000 bytes.
  add_test (NAME H5REPART-h5repart_20K COMMAND $<TARGET_FILE:h5repart> -m 20000 family_file%05d.h5 fst_family%05d.h5)
  set_tests_properties (H5REPART-h5repart_20K PROPERTIES DEPENDS H5REPART-clearall-objects)

  # repartition family member size to 5 KB.
  add_test (NAME H5REPART-h5repart_5K COMMAND $<TARGET_FILE:h5repart> -m 5k family_file%05d.h5 scd_family%05d.h5)
  set_tests_properties (H5REPART-h5repart_5K PROPERTIES DEPENDS H5REPART-clearall-objects)

  # convert family file to sec2 file of 20,000 bytes
  add_test (NAME H5REPART-h5repart_sec2 COMMAND $<TARGET_FILE:h5repart> -m 20000 -family_to_sec2 family_file%05d.h5 family_to_sec2.h5)
  set_tests_properties (H5REPART-h5repart_sec2 PROPERTIES DEPENDS H5REPART-clearall-objects)

  # test the output files repartitioned above.
  add_test (NAME H5REPART-h5repart_test COMMAND $<TARGET_FILE:h5repart_test>)
  set_tests_properties (H5REPART-h5repart_test PROPERTIES DEPENDS "H5REPART-clearall-objects;H5REPART-h5repart_20K;H5REPART-h5repart_5K;H5REPART-h5repart_sec2")

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
        h5repart_test
  )
  
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5MKGRP-clearall-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
                h5mkgrp_help.out
                h5mkgrp_help.out.err
                h5mkgrp_version.out
                h5mkgrp_version.out.err
                h5mkgrp_single.h5 
                h5mkgrp_single.out
                h5mkgrp_single.out.err
                h5mkgrp_single_v.h5 
                h5mkgrp_single_v.out
                h5mkgrp_single_v.out.err
                h5mkgrp_single_p.h5 
                h5mkgrp_single_p.out
                h5mkgrp_single_p.out.err
                h5mkgrp_single_l.h5 
                h5mkgrp_single_l.out
                h5mkgrp_single_l.out.err
                h5mkgrp_several.h5 
                h5mkgrp_several.out
                h5mkgrp_several.out.err
                h5mkgrp_several_v.h5 
                h5mkgrp_several_v.out
                h5mkgrp_several_v.out.err
                h5mkgrp_several_p.h5 
                h5mkgrp_several_p.out
                h5mkgrp_several_p.out.err
                h5mkgrp_several_l.h5 
                h5mkgrp_several_l.out
                h5mkgrp_several_l.out.err
                h5mkgrp_nested_p.h5 
                h5mkgrp_nested_p.out
                h5mkgrp_nested_p.out.err
                h5mkgrp_nested_lp.h5 
                h5mkgrp_nested_lp.out
                h5mkgrp_nested_lp.out.err
                h5mkgrp_nested_mult_p.h5 
                h5mkgrp_nested_mult_p.out
                h5mkgrp_nested_mult_p.out.err
                h5mkgrp_nested_mult_lp.h5 
                h5mkgrp_nested_mult_lp.out
                h5mkgrp_nested_mult_lp.out.err
    )
    set_tests_properties (H5MKGRP-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5MKGRP-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif (NOT "${last_test}" STREQUAL "")
    set (last_test "H5MKGRP-clearall-objects")
  endif (HDF5_ENABLE_USING_MEMCHECKER)

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
