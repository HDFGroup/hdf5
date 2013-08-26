
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  SET (LIST_HDF5_TEST_FILES
      ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/h5copy_extlinks_src.h5
      ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/h5copy_extlinks_trg.h5
      ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/h5copy_ref.h5
      ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/h5copytst.h5
  )

  SET (LIST_OTHER_TEST_FILES
      ${HDF5_TOOLS_H5COPY_SOURCE_DIR}/testfiles/h5copy_misc1.out
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  FOREACH (listfiles ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
    GET_FILENAME_COMPONENT(fname "${listfiles}" NAME)
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${fname}")
    #MESSAGE (STATUS " Copying ${listfiles}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5copy
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${listfiles} ${dest}
    )
  ENDFOREACH (listfiles ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  #
  # Perform h5copy according to passing parmeters
  #
  MACRO (ADD_H5_F_TEST testname resultcode infile fparam vparam sparam srcname dparam dstname)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      ADD_TEST (
          NAME H5COPY_F-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
              ./testfiles/${testname}.out.h5
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (
        NAME H5COPY_F-${testname}
        COMMAND $<TARGET_FILE:h5copy> -f ${fparam} -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5COPY_F-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      SET_TESTS_PROPERTIES (H5COPY_F-${testname} PROPERTIES DEPENDS H5COPY_F-${testname}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

    # resultcode=2 will cause the test to skip the diff test
    IF (NOT ${resultcode} STREQUAL "2")
      ADD_TEST (
          NAME H5COPY_F-${testname}-DIFF
          COMMAND $<TARGET_FILE:h5diff> -q ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      SET_TESTS_PROPERTIES(H5COPY_F-${testname}-DIFF PROPERTIES DEPENDS H5COPY_F-${testname})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5COPY_F-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
    ENDIF (NOT ${resultcode} STREQUAL "2")
  ENDMACRO (ADD_H5_F_TEST)
  
  MACRO (ADD_H5_TEST testname resultcode infile vparam sparam srcname dparam dstname)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      ADD_TEST (
          NAME H5COPY-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
              ./testfiles/${testname}.out.h5
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (
        NAME H5COPY-${testname}
        COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5COPY-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      SET_TESTS_PROPERTIES (H5COPY-${testname} PROPERTIES DEPENDS H5COPY-${testname}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

    # resultcode=2 will cause the test to skip the diff test
    IF (NOT ${resultcode} STREQUAL "2")
      ADD_TEST (
          NAME H5COPY-${testname}-DIFF
          COMMAND $<TARGET_FILE:h5diff> -q ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      SET_TESTS_PROPERTIES(H5COPY-${testname}-DIFF PROPERTIES DEPENDS H5COPY-${testname})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
    ENDIF (NOT ${resultcode} STREQUAL "2")
  ENDMACRO (ADD_H5_TEST)
  
  MACRO (ADD_H5_TEST2 testname resultcode infile  psparam pdparam vparam sparam srcname dparam dstname)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      ADD_TEST (
          NAME H5COPY-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
              ./testfiles/${testname}.out.h5
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (
        NAME H5COPY-${testname}-prefill
        COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5COPY-${testname}-prefill PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      SET_TESTS_PROPERTIES (H5COPY-${testname}-prefill PROPERTIES DEPENDS H5COPY-${testname}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (
        NAME H5COPY-${testname}
        COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    SET_TESTS_PROPERTIES (H5COPY-${testname} PROPERTIES DEPENDS H5COPY-${testname}-prefill)
    # resultcode=2 will cause the test to skip the diff test
    IF (NOT ${resultcode} STREQUAL "2")
      ADD_TEST (
          NAME H5COPY-${testname}-DIFF
          COMMAND $<TARGET_FILE:h5diff> -q ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      SET_TESTS_PROPERTIES(H5COPY-${testname}-DIFF PROPERTIES DEPENDS H5COPY-${testname})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
    ENDIF (NOT ${resultcode} STREQUAL "2")
  ENDMACRO (ADD_H5_TEST2)
  
  MACRO (ADD_H5_TEST_SAME testname resultcode pfile psparam pdparam vparam sparam srcname dparam dstname)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      ADD_TEST (
          NAME H5COPY_SAME-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
              ./testfiles/${testname}.out.h5
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (
        NAME H5COPY_SAME-${testname}-prefill
        COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${pfile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5COPY_SAME-${testname}-prefill PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      SET_TESTS_PROPERTIES (H5COPY_SAME-${testname}-prefill PROPERTIES DEPENDS H5COPY_SAME-${testname}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (
        NAME H5COPY_SAME-${testname}
        COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${testname}.out.h5 -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    SET_TESTS_PROPERTIES (H5COPY_SAME-${testname} PROPERTIES DEPENDS H5COPY_SAME-${testname}-prefill)
    # resultcode=2 will cause the test to skip the diff test
    IF (NOT ${resultcode} STREQUAL "2")
      ADD_TEST (
          NAME H5COPY_SAME-${testname}-DIFF
          COMMAND $<TARGET_FILE:h5diff> -q ./testfiles/${testname}.out.h5 ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      SET_TESTS_PROPERTIES(H5COPY_SAME-${testname}-DIFF PROPERTIES DEPENDS H5COPY_SAME-${testname})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5COPY_SAME-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
    ENDIF (NOT ${resultcode} STREQUAL "2")
  ENDMACRO (ADD_H5_TEST_SAME)

  #
  # Similiar to ADD_H5_TEST macro. Compare to outputs from source & target
  # files instead of checking with h5ls.
  #
  MACRO (ADD_H5_CMP_TEST testname resultcode infile vparam sparam srcname dparam dstname)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5COPY-CMP-${testname} COMMAND $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN})
      IF (${resultcode} STREQUAL "1")
        SET_TESTS_PROPERTIES (H5COPY-CMP-${testname} PROPERTIES WILL_FAIL "true")
      ENDIF (${resultcode} STREQUAL "1")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5COPY-CMP-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      ADD_TEST (
          NAME H5COPY-CMP-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
              ./testfiles/${testname}.out.h5
              ./testfiles/${testname}.out.out
              ./testfiles/${testname}.out.out.err
      )
      ADD_TEST (
          NAME H5COPY-CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
              -D "TEST_ARGS=-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${vparam};${sparam};${srcname};${dparam};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out"
              -D "TEST_MASK=true"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5COPY-CMP-${testname} PROPERTIES DEPENDS H5COPY-CMP-${testname}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_CMP_TEST)

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

  IF (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    ADD_TEST (
        NAME H5COPY-clearall-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            simple.out.h5
            chunk.out.h5
            compact.out.h5
            compound.out.h5
            compressed.out.h5
            named_vl.out.h5
            nested_vl.out.h5
            simple_top.out.h5
            dsrename.out.h5
            grp_empty.out.h5
            grp_dsets.out.h5
            grp_nested.out.h5
            simple_group.out.h5
            grp_rename.out.h5
            grp_dsets_rename.out.h5
            A_B1_simple.out.h5
            A_B2_simple2.out.h5
            C_D_simple.out.h5
            E_F_grp_dsets.out.h5
            G_H_grp_nested.out.h5
            region_ref.out.h5
            ext_link.out.h5
            ext_link_f.out.h5
            ext_dangle_noobj.out.h5
            ext_dangle_noobj_f.out.h5
            ext_dangle_nofile.out.h5
            ext_dangle_nofile_f.out.h5
            ext_link_group.out.h5
            ext_link_group_f.out.h5
            samefile1.out.h5
            samefile2.out.h5
            h5copy_misc1.out.h5
            h5copy_misc1.out.out
            h5copy_misc1.out.out.err
    )
    SET_TESTS_PROPERTIES (H5COPY-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5COPY-clearall-objects PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5COPY-clearall-objects")
  ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  
  # "Test copying various forms of datasets"
  ADD_H5_TEST (simple 0 ${HDF_FILE1}.h5 -v -s simple -d simple)
  ADD_H5_TEST (chunk 0 ${HDF_FILE1}.h5 -v -s chunk -d chunk)
  ADD_H5_TEST (compact 0 ${HDF_FILE1}.h5 -v -s compact -d compact)
  ADD_H5_TEST (compound 0 ${HDF_FILE1}.h5 -v -s compound -d compound)
  ADD_H5_TEST (compressed 0 ${HDF_FILE1}.h5 -v -s compressed -d compressed)
  ADD_H5_TEST (named_vl 0 ${HDF_FILE1}.h5 -v -s named_vl -d named_vl)
  ADD_H5_TEST (nested_vl 0 ${HDF_FILE1}.h5 -v -s nested_vl -d nested_vl)

  # "Test copying dataset within group in source file to root of destination"
  ADD_H5_TEST (simple_top 0 ${HDF_FILE1}.h5 -v -s grp_dsets/simple -d simple_top)

  # "Test copying & renaming dataset"
  ADD_H5_TEST (dsrename 0 ${HDF_FILE1}.h5 -v -s compound -d rename)

  # "Test copying empty, 'full' & 'nested' groups"
  ADD_H5_TEST (grp_empty 0 ${HDF_FILE1}.h5 -v -s grp_empty -d grp_empty)
  ADD_H5_TEST (grp_dsets 0 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_dsets)
  ADD_H5_TEST (grp_nested 0 ${HDF_FILE1}.h5 -v -s grp_nested -d grp_nested)

  # "Test copying dataset within group in source file to group in destination"
  ADD_H5_TEST2 (simple_group 0 ${HDF_FILE1}.h5 grp_dsets grp_dsets -v -s /grp_dsets/simple -d /grp_dsets/simple_group)

  # "Test copying & renaming group"
  ADD_H5_TEST (grp_rename 0 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_rename)

  # "Test copying 'full' group hierarchy into group in destination file"
  ADD_H5_TEST2 (grp_dsets_rename 0 ${HDF_FILE1}.h5 grp_dsets grp_rename -v -s grp_dsets -d /grp_rename/grp_dsets)

  # "Test copying objects into group hier. that doesn't exist yet in destination file"
  ADD_H5_TEST (A_B1_simple 0 ${HDF_FILE1}.h5 -vp -s simple -d /A/B1/simple)
  ADD_H5_TEST (A_B2_simple2 0 ${HDF_FILE1}.h5 -vp -s simple -d /A/B2/simple2)
  ADD_H5_TEST (C_D_simple 0 ${HDF_FILE1}.h5 -vp -s /grp_dsets/simple -d /C/D/simple)
  ADD_H5_TEST (E_F_grp_dsets 0 ${HDF_FILE1}.h5 -vp -s /grp_dsets -d /E/F/grp_dsets)
  ADD_H5_TEST (G_H_grp_nested 0 ${HDF_FILE1}.h5 -vp -s /grp_nested -d /G/H/grp_nested)
  
############# COPY REFERENCES ##############

  # "Test copying object and region references"
  ADD_H5_F_TEST (region_ref 2 ${HDF_FILE2}.h5 ref -v -s / -d /COPY)

############# COPY EXT LINKS ############## 

  # "Test copying external link directly without -f ext"
  ADD_H5_TEST (ext_link 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_dset -d /copy1_dset)

  # "Test copying external link directly with -f ext"
  ADD_H5_F_TEST (ext_link_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_dset -d /copy2_dset)

  # "Test copying dangling external link (no obj) directly without -f ext"
  ADD_H5_TEST (ext_dangle_noobj 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_1)

  # "Test copying dangling external link (no obj) directly with -f ext"
  ADD_H5_F_TEST (ext_dangle_noobj_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_2)

  # "Test copying dangling external link (no file) directly without -f ext"
  ADD_H5_TEST (ext_dangle_nofile 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_1)

  # "Test copying dangling external link (no file) directly with -f ext"
  ADD_H5_F_TEST (ext_dangle_nofile_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_2)

  # "Test copying a group contains external links without -f ext"
  ADD_H5_TEST (ext_link_group 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext -d /copy1_group)

  # "Test copying a group contains external links with -f ext"
  ADD_H5_F_TEST (ext_link_group_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext -d /copy2_group) 

############# Test misc. ############## 

  #-----------------------------------------------------------------
  # "Test copying object into group which doesn't exist, without -p"
  #
  ADD_H5_CMP_TEST (h5copy_misc1 1 ${HDF_FILE1}.h5 -v -s /simple -d /g1/g2/simple)
  
  #-------------------------------------------
  # "Test copying objects to the same file "
  #
  # - dataset
  ADD_H5_TEST_SAME (samefile1 0 ${HDF_FILE1}.h5 /simple /simple -v -s /simple -d /simple_cp)
  # - group with some datasets
  ADD_H5_TEST_SAME (samefile2 0 ${HDF_FILE1}.h5 /grp_dsets /grp_dsets -v -s /grp_dsets -d /grp_dsets_cp)
