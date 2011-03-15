cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5COPY)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5copy and test executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5copy ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copy.c)
TARGET_NAMING (h5copy ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5copy  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5copy)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5copygentest ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/h5copygentest.c)
    TARGET_NAMING (h5copygentest ${LIB_TYPE})
    TARGET_LINK_LIBRARIES (h5copygentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

    #ADD_TEST (NAME h5copygentest COMMAND $<TARGET_FILE:h5copygentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      h5copy_extlinks_src.out.ls
      h5copy_ref.out.ls
      h5copytst.out.ls
  )
  SET (HDF5_REFERENCE_TEST_FILES
      h5copy_extlinks_src.h5
      h5copy_extlinks_trg.h5
      h5copy_ref.h5
      h5copytst.h5
      h5copy_misc1.out
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  FOREACH (ls_file ${HDF5_REFERENCE_FILES})
    SET (lsdest "${PROJECT_BINARY_DIR}/testfiles/${ls_file}")
    #MESSAGE (STATUS " Translating ${ls_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5copy
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/${ls_file} ${lsdest} -l3
    )
  ENDFOREACH (ls_file ${HDF5_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5copy
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  #
  # Perform h5copy according to passing parmeters
  #
  MACRO (ADD_H5COPY_TEST testname resultcode infile outfile vparam srcname dstname)
  
    IF (NOT "${ARGN}" STREQUAL "")
      ADD_TEST (
          NAME H5COPY-${testname}
          COMMAND $<TARGET_FILE:h5copy> -f ${ARGN} -i ./testfiles/${infile} -o ./testfiles/${outfile} -${vparam} -s ${srcname} -d ${dstname}
      )
    ELSE (NOT "${ARGN}" STREQUAL "")
      ADD_TEST (
          NAME H5COPY-${testname}
          COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${outfile} -${vparam} -s ${srcname} -d ${dstname}
      )
    ENDIF (NOT "${ARGN}" STREQUAL "")
    # resultcode=2 will cause the test to skip the diff test
    IF (NOT ${resultcode} STREQUAL "2")
      ADD_TEST (
          NAME H5COPY-DIFF_${testname}
          COMMAND $<TARGET_FILE:h5diff> -q ./testfiles/${infile} ./testfiles/${outfile} ${srcname} ${dstname}
      )
      SET_TESTS_PROPERTIES(H5COPY-DIFF_${testname} PROPERTIES DEPENDS H5COPY-${testname})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5COPY-DIFF_${testname} PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
    ENDIF (NOT ${resultcode} STREQUAL "2")
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5COPY-${testname} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5COPY-DIFF_${testname}")
  ENDMACRO (ADD_H5COPY_TEST)

  #
  # call ADD_H5COPY_TEST macro with assumption that source and target file
  # is different
  #
  MACRO (ADD_H5_TEST testname resultcode testfile vparam srcname dstname)
    ADD_H5COPY_TEST (${testname} ${resultcode} ${testfile}.h5 ${testfile}.out.h5 ${vparam} ${srcname} ${dstname} ${ARGN})
  ENDMACRO (ADD_H5_TEST testname resultcode testfile vparam srcname dstname)

  #
  # Test result with h5ls
  #
  MACRO (ADD_H5LS_TEST file filetest)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5COPY-H5LS_${file}-${filetest} COMMAND $<TARGET_FILE:h5ls> -Svr ./testfiles/${file}.out.h5)
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5COPY-H5LS_${file}-${filetest}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5ls>"
              -D "TEST_ARGS=-Svr;./testfiles/${file}.out.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${file}.out.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=./testfiles/${file}.out.ls"
              -D "TEST_MASK=true"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5COPY-H5LS_${file}-${filetest} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5COPY-H5LS_${file}-${filetest}")
  ENDMACRO (ADD_H5LS_TEST file filetest)

  #
  # Similiar to ADD_H5_TEST macro. Compare to outputs from source & target
  # files instead of checking with h5ls.
  #
  MACRO (ADD_H5_CMP_TEST testname resultcode testfile vparam srcname dstname)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5COPY-CMP-${testname} COMMAND $<TARGET_FILE:h5copy> -i ${testfile}.h5 -o ./testfiles/${testfile}.out.h5 -${vparam} -s ${srcname} -d ${dstname})
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5COPY-CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
              -D "TEST_ARGS=-i;${testfile}.h5;-o;./testfiles/${testfile}.out.h5;-${vparam};-s;${srcname};-d;${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out"
              -D "TEST_MASK=true"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5COPY-CMP-${testname} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5COPY-CMP-${testname}")
  ENDMACRO (ADD_H5_CMP_TEST testname resultcode testfile vparam srcname dstname)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # test file names 
  # --------------------------------------------------------------------
  SET (HDF_FILE1 h5copytst)
  SET (HDF_FILE2 h5copy_ref)
  SET (HDF_EXT_SRC_FILE h5copy_extlinks_src)
  SET (HDF_EXT_TRG_FILE h5copy_extlinks_trg)
  SET (HDF_FILE1_TMP h5copytst_tmp)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5COPY-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          ./testfiles/${HDF_FILE1}.out.h5
          ./testfiles/${HDF_FILE1}.out.out
          ./testfiles/${HDF_FILE1}.out.out.err
          ./testfiles/${HDF_FILE2}.out.h5
          ./testfiles/${HDF_FILE2}.out.out
          ./testfiles/${HDF_FILE2}.out.out.err
          ./testfiles/${HDF_EXT_SRC_FILE}.out.h5
          ./testfiles/${HDF_EXT_SRC_FILE}.out.out
          ./testfiles/${HDF_EXT_SRC_FILE}.out.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5COPY-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5COPY-clearall-objects")
  
  # "Test copying various forms of datasets"
  ADD_H5_TEST (simple 0 ${HDF_FILE1} v simple simple)
  ADD_H5_TEST (chunk 0 ${HDF_FILE1} v chunk chunk)
  ADD_H5_TEST (compact 0 ${HDF_FILE1} v compact compact)
  ADD_H5_TEST (compound 0 ${HDF_FILE1} v compound compound)
  ADD_H5_TEST (compressed 0 ${HDF_FILE1} v compressed compressed)
  ADD_H5_TEST (named_vl 0 ${HDF_FILE1} v named_vl named_vl)
  ADD_H5_TEST (nested_vl 0 ${HDF_FILE1} v nested_vl nested_vl)

  # "Test copying dataset within group in source file to root of destination"
  ADD_H5_TEST (simple_top 0 ${HDF_FILE1} v grp_dsets/simple simple_top)

  # "Test copying & renaming dataset"
  ADD_H5_TEST (rename 0 ${HDF_FILE1} v compound rename)

  # "Test copying empty, 'full' & 'nested' groups"
  ADD_H5_TEST (grp_empty 0 ${HDF_FILE1} v grp_empty grp_empty)
  ADD_H5_TEST (grp_dsets 0 ${HDF_FILE1} v grp_dsets grp_dsets)
  ADD_H5_TEST (grp_nested 0 ${HDF_FILE1} v grp_nested grp_nested)

  # "Test copying dataset within group in source file to group in destination"
  ADD_H5_TEST (simple_group 0 ${HDF_FILE1} v /grp_dsets/simple /grp_dsets/simple_group)

  # "Test copying & renaming group"
  ADD_H5_TEST (grp_rename 0 ${HDF_FILE1} v grp_dsets grp_rename)

  # "Test copying 'full' group hierarchy into group in destination file"
  ADD_H5_TEST (grp_dsets_rename 0 ${HDF_FILE1} v grp_dsets /grp_rename/grp_dsets)

  # "Test copying objects into group hier. that doesn't exist yet in destination file"
  ADD_H5_TEST (A_B1_simple 0 ${HDF_FILE1} vp simple /A/B1/simple)
  ADD_H5_TEST (A_B2_simple2 0 ${HDF_FILE1} vp simple /A/B2/simple2)
  ADD_H5_TEST (C_D_simple 0 ${HDF_FILE1} vp /grp_dsets/simple /C/D/simple)
  ADD_H5_TEST (E_F_grp_dsets 0 ${HDF_FILE1} vp /grp_dsets /E/F/grp_dsets)
  ADD_H5_TEST (G_H_grp_nested 0 ${HDF_FILE1} vp /grp_nested /G/H/grp_nested)

  # Verify that the file created above is correct
  ADD_H5LS_TEST (${HDF_FILE1} basic)
  
############# COPY REFERENCES ##############

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5COPY-clear-refs
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          ./testfiles/${HDF_FILE2}.out.h5
          ./testfiles/${HDF_FILE2}.out.out
          ./testfiles/${HDF_FILE2}.out.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5COPY-clear-refs PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5COPY-clear-refs")

  # "Test copying object and region references"
  ADD_H5_TEST (region_ref 2 ${HDF_FILE2} v / /COPY ref)

  # Verify that the file created above is correct
  ADD_H5LS_TEST (${HDF_FILE2} refs)

############# COPY EXT LINKS ############## 

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5COPY-clear-ext-links
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          ./testfiles/${HDF_EXT_SRC_FILE}.out.h5
          ./testfiles/${HDF_EXT_SRC_FILE}.out.out
          ./testfiles/${HDF_EXT_SRC_FILE}.out.out.err
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5COPY-clear-ext-links PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5COPY-clear-ext-links")

  # "Test copying external link directly without -f ext"
  ADD_H5_TEST (ext_link 2 ${HDF_EXT_SRC_FILE} v /group_ext/extlink_dset /copy1_dset)

  # "Test copying external link directly with -f ext"
  ADD_H5_TEST (ext_link_f 2 ${HDF_EXT_SRC_FILE} v /group_ext/extlink_dset /copy2_dset ext)

  # "Test copying dangling external link (no obj) directly without -f ext"
  ADD_H5_TEST (ext_dangle_noobj 0 ${HDF_EXT_SRC_FILE} v /group_ext/extlink_notyet1 /copy_dangle1_1)

  # "Test copying dangling external link (no obj) directly with -f ext"
  ADD_H5_TEST (ext_dangle_noobj_f 2 ${HDF_EXT_SRC_FILE} v /group_ext/extlink_notyet1 /copy_dangle1_2 ext)

  # "Test copying dangling external link (no file) directly without -f ext"
  ADD_H5_TEST (ext_dangle_nofile 0 ${HDF_EXT_SRC_FILE} v /group_ext/extlink_notyet2 /copy_dangle2_1)

  # "Test copying dangling external link (no file) directly with -f ext"
  ADD_H5_TEST (ext_dangle_nofile_f 2 ${HDF_EXT_SRC_FILE} v /group_ext/extlink_notyet2 /copy_dangle2_2 ext)

  # "Test copying a group contains external links without -f ext"
  ADD_H5_TEST (ext_link_group 2 ${HDF_EXT_SRC_FILE} v /group_ext /copy1_group)

  # "Test copying a group contains external links with -f ext"
  ADD_H5_TEST (ext_link_group_f 2 ${HDF_EXT_SRC_FILE} v /group_ext /copy2_group ext) 

  # Verify that the file created above is correct
  ADD_H5LS_TEST (${HDF_EXT_SRC_FILE} links)

############# Test misc. ############## 

  ADD_CUSTOM_COMMAND (
      TARGET     h5copy
      POST_BUILD
      COMMAND    ${CMAKE_COMMAND}
      ARGS       -E copy_if_different ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/h5copytst.h5 ${PROJECT_BINARY_DIR}/h5copytst.h5
  )

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5COPY-clear-misc
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          ./testfiles/${HDF_FILE1}.out.h5
          ./testfiles/${HDF_FILE1}.out.out
          ./testfiles/${HDF_FILE1}.out.out.err
          ./testfiles/${HDF_FILE1_TMP}.h5
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5COPY-clear-misc PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5COPY-clear-misc")

  #-----------------------------------------------------------------
  # "Test copying object into group which doesn't exist, without -p"
  #
  ADD_H5_CMP_TEST (h5copy_misc1 1 ${HDF_FILE1} v /simple /g1/g2/simple)
  
  #-------------------------------------------
  # "Test copying objects to the same file "
  #
  # create temporary test file with some objects
  ADD_H5COPY_TEST (samefile1_pre 0 ${HDF_FILE1}.h5 ${HDF_FILE1_TMP}.h5 v /simple /simple)
  ADD_H5COPY_TEST (samefile2_pre 0 ${HDF_FILE1}.h5 ${HDF_FILE1_TMP}.h5 v /grp_dsets /grp_dsets)
  # actual test cases
  # - dataset
  ADD_H5COPY_TEST (samefile1 0 ${HDF_FILE1_TMP}.h5 ${HDF_FILE1_TMP}.h5 v /simple /simple_cp)
  # - group with some datasets
  ADD_H5COPY_TEST (samefile2 0 ${HDF_FILE1_TMP}.h5 ${HDF_FILE1_TMP}.h5 v /grp_dsets /grp_dsets_cp)
ENDIF (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5copy
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
