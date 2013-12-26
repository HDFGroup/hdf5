
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  IF (HDF5_TEST_VFD)
    SET (VFD_LIST
        sec2
        stdio
        core
        split
        multi
        family
    )
  
    IF (DIRECT_VFD)
      SET (VFD_LIST ${VFD_LIST} direct)
    ENDIF (DIRECT_VFD)

    MACRO (ADD_VFD_TEST vfdname resultcode)
      ADD_TEST (
        NAME H5REPACK-VFD-${vfdname}-h5repacktest 
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5repacktest>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=h5repacktest"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK-VFD-${vfdname}-h5repacktest PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5REPACK-VFD-${vfdname}-h5repacktest")
    ENDMACRO (ADD_VFD_TEST)
  ENDIF (HDF5_TEST_VFD)
    
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  SET (LIST_HDF5_TEST_FILES
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_attr.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_attr_refs.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_deflate.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_early.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_ext.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_fill.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_filters.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_fletcher.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_hlink.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layouto.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout2.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout3.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout.UD.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_named_dtypes.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_nbit.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_objs.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_refs.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_shuffle.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_soffset.h5
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_szip.h5
      # h5diff/testfile
      ${HDF5_TOOLS_H5DIFF_SOURCE_DIR}/testfiles/h5diff_attr1.h5
      # tools/testfiles
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00000.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00001.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00002.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00003.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00004.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00005.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00006.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00007.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00008.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00009.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tfamily00010.h5
  )

  SET (LIST_OTHER_TEST_FILES
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack-help.txt
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_ext.bin
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/ublock.bin
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack.info
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/deflate_limit.h5repack_layout.h5.ddl
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout.h5.ddl
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_filters.h5.tst
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout.h5-plugin_test.ddl
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/plugin_test.h5repack_layout.h5.tst
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/h5repack_layout.UD.h5-plugin_none.ddl
      ${HDF5_TOOLS_H5REPACK_SOURCE_DIR}/testfiles/plugin_none.h5repack_layout.UD.h5.tst
  )

  FOREACH (h5_file ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
    GET_FILENAME_COMPONENT(fname "${h5_file}" NAME)
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${fname}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5repack
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_HELP_TEST testname resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5REPACK-${testname} COMMAND $<TARGET_FILE:h5repack> ${ARGN})
      SET_TESTS_PROPERTIES (H5REPACK-${testname} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5REPACK-${testname}")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5REPACK-h5repack-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove h5repack-${testname}.out h5repack-${testname}.out.err
      )
      SET_TESTS_PROPERTIES (H5REPACK-h5repack-${testname}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      ADD_TEST (
          NAME H5REPACK-h5repack-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=h5repack-${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=h5repack-${testname}.txt"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5REPACK-h5repack-${testname} PROPERTIES DEPENDS "H5REPACK-h5repack-${testname}-clear-objects")
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_HELP_TEST)

  MACRO (ADD_H5_TEST_OLD testname testtype testfile)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_OLD-${testname}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} -i ${PROJECT_BINARY_DIR}/testfiles/${testfile} -o ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_TEST (
          NAME H5REPACK_OLD-${testname}
          COMMAND $<TARGET_FILE:h5repack> ${ARGN} -i ${PROJECT_BINARY_DIR}/testfiles/${testfile} -o ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK_OLD-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      ADD_TEST (
          NAME H5REPACK_OLD-${testname}_DFF
          COMMAND $<TARGET_FILE:h5diff> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      SET_TESTS_PROPERTIES (H5REPACK_OLD-${testname}_DFF PROPERTIES DEPENDS H5REPACK_OLD-${testname})
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_TEST_OLD)

  MACRO (ADD_H5_TEST testname testtype testfile)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK-${testname}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      ADD_TEST (
          NAME H5REPACK-${testname}
          COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      ADD_TEST (
          NAME H5REPACK-${testname}_DFF
          COMMAND $<TARGET_FILE:h5diff> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      SET_TESTS_PROPERTIES (H5REPACK-${testname}_DFF PROPERTIES DEPENDS H5REPACK-${testname})
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_TEST)

  MACRO (ADD_H5_CMP_TEST testname testfilter testtype resultcode resultfile)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_CMP-${testname}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      # If using memchecker add tests without using scripts
      IF (HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_CMP-${testname}
            COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile})
      ELSE (HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_CMP-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
                -D "TEST_ARGS:STRING=${ARGN};testfiles/${resultfile};testfiles/out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_FILTER:STRING=${testfilter}"
                -D "TEST_REFERENCE=testfiles/${resultfile}.tst"
                -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
        )
      ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK_CMP-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_CMP_TEST)

  MACRO (ADD_H5_DMP_TEST testname testtype resultcode resultfile)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_DMP-${testname}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      # If using memchecker add tests without using scripts
      ADD_TEST (
          NAME H5REPACK_DMP-${testname}
          COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile})
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK_DMP-${testname} PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_DMP-h5dump-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=-pH;./testfiles/out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=testfiles/${testname}.${resultfile}.ddl"
                -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
        )
        SET_TESTS_PROPERTIES (H5REPACK_DMP-h5dump-${testname} PROPERTIES DEPENDS "H5REPACK_DMP-${testname}")
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_DMP_TEST)

  MACRO (ADD_H5_VERIFY_TEST testname testtype resultcode testfile testdset testfilter)
    IF (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}-SKIPPED
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP -d ${testdset} -pH ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ELSE (${testtype} STREQUAL "SKIP")
      IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
        ADD_TEST (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}
            COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        IF (NOT "${last_test}" STREQUAL "")
          SET_TESTS_PROPERTIES (H5REPACK_VERIFY_LAYOUT-${testname} PROPERTIES DEPENDS ${last_test})
        ENDIF (NOT "${last_test}" STREQUAL "")
        ADD_TEST (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}_DFF
            COMMAND $<TARGET_FILE:h5diff> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        SET_TESTS_PROPERTIES (H5REPACK_VERIFY_LAYOUT-${testname}_DFF PROPERTIES DEPENDS H5REPACK_VERIFY_LAYOUT-${testname})
        IF (${resultcode} STREQUAL "0")
          ADD_TEST (
              NAME H5REPACK_VERIFY_LAYOUT-${testname}_DMP
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                  -D "TEST_ARGS:STRING=-d;${testdset};-pH;testfiles/out-${testname}.${testfile}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                  -D "TEST_OUTPUT=./testfiles/${testfile}-${testname}-v.out"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_FILTER:STRING=${testfilter}"
                  -D "TEST_REFERENCE=${testfilter}"
                  -P "${HDF5_RESOURCES_DIR}/grepTest.cmake"
          )
          SET_TESTS_PROPERTIES (H5REPACK_VERIFY_LAYOUT-${testname}_DMP PROPERTIES DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}_DFF)
        ELSE (${resultcode} STREQUAL "0")
          IF (${testfilter} STREQUAL "CHUNKED")
            SET (nottestfilter "(CONTIGUOUS|COMPACT)")
          ENDIF (${testfilter} STREQUAL "CHUNKED")
          IF (${testfilter} STREQUAL "CONTIGUOUS")
            SET (nottestfilter "(CHUNK|COMPACT)")
          ENDIF (${testfilter} STREQUAL "CONTIGUOUS")
          IF (${testfilter} STREQUAL "COMPACT")
            SET (nottestfilter "(CONTIGUOUS|CHUNK)")
          ENDIF (${testfilter} STREQUAL "COMPACT")
          ADD_TEST (
              NAME H5REPACK_VERIFY_LAYOUT-${testname}_DMP
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                  -D "TEST_ARGS:STRING=-pH;testfiles/out-${testname}.${testfile}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                  -D "TEST_OUTPUT=./testfiles/${testfile}-${testname}-v.out"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_FILTER:STRING=${nottestfilter}"
                  -D "TEST_REFERENCE=${testfilter}"
                  -P "${HDF5_RESOURCES_DIR}/grepTest.cmake"
          )
          SET_TESTS_PROPERTIES (H5REPACK_VERIFY_LAYOUT-${testname}_DMP PROPERTIES DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}_DFF)
        ENDIF (${resultcode} STREQUAL "0")
      ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ENDIF (${testtype} STREQUAL "SKIP")
  ENDMACRO (ADD_H5_VERIFY_TEST)

  MACRO (ADD_H5_TEST_META testname testfile)
      ADD_TEST (
          NAME H5REPACK_META-${testname}_N
          COMMAND $<TARGET_FILE:h5repack> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_N.${testname}.h5
      )
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5REPACK_META-${testname}_N PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      ADD_TEST (
          NAME H5REPACK_META-${testname}_M
          COMMAND $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_M.${testname}.h5
      )
      SET_TESTS_PROPERTIES (H5REPACK_META-${testname}_M PROPERTIES DEPENDS H5REPACK_META-${testname}_N)

      ADD_TEST (NAME H5REPACK_META-${testname} COMMAND ${CMAKE_COMMAND} -E compare_files ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_N.${testname}.h5 ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_M.${testname}.h5)
      SET_TESTS_PROPERTIES (H5REPACK_META-${testname} PROPERTIES WILL_FAIL "true")
      SET_TESTS_PROPERTIES (H5REPACK_META-${testname} PROPERTIES DEPENDS H5REPACK_META-${testname}_M)
  ENDMACRO (ADD_H5_TEST_META)

  MACRO (ADD_H5_UD_TEST testname resultcode resultfile)
    IF (HDF5_BUILD_TOOLS AND NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      ADD_TEST (
          NAME H5REPACK_UD-${testname}-clearall-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove 
              testfiles/out-${testname}.${resultfile}
              testfiles/${testname}.${resultfile}.out
              testfiles/${testname}.${resultfile}.out.err
              testfiles/${resultfile}-${testname}.out
              testfiles/${resultfile}-${testname}.out.err
      )
      ADD_TEST (
          NAME H5REPACK_UD-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
              -D "TEST_ARGS:STRING=${ARGN};${PROJECT_BINARY_DIR}/testfiles/${resultfile};${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_FILTER:STRING=O?...ing file[^\n]+\n"
              -D "TEST_OUTPUT=./testfiles/${testname}.${resultfile}.out"
              -D "TEST_REFERENCE=testfiles/${testname}.${resultfile}.tst"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5REPACK_UD-${testname} PROPERTIES DEPENDS H5REPACK_UD-${testname}-clearall-objects)
      ADD_TEST (
          NAME H5REPACK_UD-h5dump-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=-pH;testfiles/out-${testname}.${resultfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${resultfile}-${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=testfiles/${resultfile}-${testname}.ddl"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5REPACK_UD-h5dump-${testname} PROPERTIES DEPENDS "H5REPACK_UD-${testname}")
    ENDIF (HDF5_BUILD_TOOLS AND NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_UD_TEST)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # test file names 
  # --------------------------------------------------------------------
  SET (INFO_FILE testfiles/h5repack.info)

  SET (FILE0 h5repack_fill.h5)
  SET (FILE1 h5repack_objs.h5)
  SET (FILE2 h5repack_attr.h5)
  SET (FILE3 h5repack_hlink.h5)
  SET (FILE4 h5repack_layout.h5)
  SET (FILE5 h5repack_early.h5)
  SET (FILE7 h5repack_szip.h5)
  SET (FILE8 h5repack_deflate.h5)
  SET (FILE9 h5repack_shuffle.h5)
  SET (FILE10 h5repack_fletcher.h5)
  SET (FILE11 h5repack_filters.h5)
  SET (FILE12 h5repack_nbit.h5)
  SET (FILE13 h5repack_soffset.h5)
  SET (FILE14 h5repack_layouto.h5 )     # A file with an older version of the layout message (copy of test/tlayouto.h5)
  SET (FILE15 h5repack_named_dtypes.h5)
  SET (FILE16 tfamily%05d.h5)           # located in common testfiles folder
  SET (FILE18 h5repack_layout2.h5)
  SET (FILE_REF h5repack_refs.h5)
  SET (FILE_ATTR_REF h5repack_attr_refs.h5)
  
  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME H5REPACK-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
         ./testfiles/h5dump-help.out
         ./testfiles/h5repack_filters.h5-gzip_verbose_filters.out
         ./testfiles/h5repack_filters.h5-gzip_verbose_filters.out.err
         ./testfiles/h5repack_layout.h5-chunk_18x13-v.out
         ./testfiles/h5repack_layout.h5-chunk_18x13-v.out.err
         ./testfiles/h5repack_layout.h5-chunk_20x10-v.out
         ./testfiles/h5repack_layout.h5-chunk_20x10-v.out.err
         ./testfiles/h5repack_layout.h5-chunk_compa-v.out
         ./testfiles/h5repack_layout.h5-chunk_compa-v.out.err
         ./testfiles/h5repack_layout.h5-chunk_conti-v.out
         ./testfiles/h5repack_layout.h5-chunk_conti-v.out.err
         ./testfiles/h5repack_layout.h5-compa-v.out
         ./testfiles/h5repack_layout.h5-compa-v.out.err
         ./testfiles/h5repack_layout.h5-conti-v.out
         ./testfiles/h5repack_layout.h5-conti-v.out.err
         ./testfiles/h5repack_layout.h5-deflate_limit.out
         ./testfiles/h5repack_layout.h5-deflate_limit.out.err
         ./testfiles/h5repack_layout.h5-dset2_chunk_20x10-v.out
         ./testfiles/h5repack_layout.h5-dset2_chunk_20x10-v.out.err
         ./testfiles/h5repack_layout.h5-dset2_compa-v.out
         ./testfiles/h5repack_layout.h5-dset2_compa-v.out.err
         ./testfiles/h5repack_layout.h5-dset2_conti-v.out
         ./testfiles/h5repack_layout.h5-dset2_conti-v.out.err
         ./testfiles/h5repack_layout.h5-dset_compa_chunk-v.out
         ./testfiles/h5repack_layout.h5-dset_compa_chunk-v.out.err
         ./testfiles/h5repack_layout.h5-dset_compa_compa-v.out
         ./testfiles/h5repack_layout.h5-dset_compa_compa-v.out.err
         ./testfiles/h5repack_layout.h5-dset_compa_conti-v.out
         ./testfiles/h5repack_layout.h5-dset_compa_conti-v.out.err
         ./testfiles/h5repack_layout.h5-dset_conti_chunk-v.out
         ./testfiles/h5repack_layout.h5-dset_conti_chunk-v.out.err
         ./testfiles/h5repack_layout.h5-dset_conti_compa-v.out
         ./testfiles/h5repack_layout.h5-dset_conti_compa-v.out.err
         ./testfiles/h5repack_layout.h5-dset_conti_conti-v.out
         ./testfiles/h5repack_layout.h5-dset_conti_conti-v.out.err
         ./testfiles/h5repack_layout.h5-layout_long_switches-v.out
         ./testfiles/h5repack_layout.h5-layout_long_switches-v.out.err
         ./testfiles/h5repack_layout.h5-layout_short_switches-v.out
         ./testfiles/h5repack_layout.h5-layout_short_switches-v.out.err
         ./testfiles/h5repack_layout.h5-plugin_test.out
         ./testfiles/h5repack_layout.h5-plugin_test.out.err
         ./testfiles/h5repack_layout2.h5-contig_small_compa-v.out
         ./testfiles/h5repack_layout2.h5-contig_small_compa-v.out.err
         ./testfiles/h5repack_layout2.h5-contig_small_fixed_compa-v.out
         ./testfiles/h5repack_layout2.h5-contig_small_fixed_compa-v.out.err
         ./testfiles/h5repack_layout3.h5-ckdim_biger-v.out
         ./testfiles/h5repack_layout3.h5-ckdim_biger-v.out.err
         ./testfiles/h5repack_layout3.h5-ckdim_smaller-v.out
         ./testfiles/h5repack_layout3.h5-ckdim_smaller-v.out.err
         ./testfiles/h5repack_layout3.h5-chunk2chunk-v.out
         ./testfiles/h5repack_layout3.h5-chunk2chunk-v.out.err
         ./testfiles/h5repack_layout3.h5-chunk2compa-v.out
         ./testfiles/h5repack_layout3.h5-chunk2compa-v.out.err
         ./testfiles/h5repack_layout3.h5-chunk2conti-v.out
         ./testfiles/h5repack_layout3.h5-chunk2conti-v.out.err
         ./testfiles/h5repack_layout3.h5-error1-v.out
         ./testfiles/h5repack_layout3.h5-error1-v.out.err
         ./testfiles/h5repack_layout3.h5-error2-v.out
         ./testfiles/h5repack_layout3.h5-error2-v.out.err
         ./testfiles/h5repack_layout3.h5-error3-v.out
         ./testfiles/h5repack_layout3.h5-error3-v.out.err
         ./testfiles/out-family.tfamily%05d.h5
         ./testfiles/out-HDFFV-7840.h5diff_attr1.h5
         ./testfiles/out-attr.h5repack_attr.h5
         ./testfiles/out-native_attr.h5repack_attr.h5
         ./testfiles/out-HDFFV-5932.h5repack_attr_refs.h5
         ./testfiles/out-deflate_copy.h5repack_deflate.h5
         ./testfiles/out-deflate_remove.h5repack_deflate.h5
         ./testfiles/out-early.h5repack_early.h5
         ./testfiles/out-fill.h5repack_fill.h5
         ./testfiles/out-native_fill.h5repack_fill.h5
         ./testfiles/out-gzip_verbose_filters.h5repack_filters.h5
         ./testfiles/out-fletcher_copy.h5repack_fletcher.h5
         ./testfiles/out-fletcher_remove.h5repack_fletcher.h5
         ./testfiles/out-hlink.h5repack_hlink.h5
         ./testfiles/out-chunk_18x13.h5repack_layout.h5
         ./testfiles/out-chunk_20x10.h5repack_layout.h5
         ./testfiles/out-chunk_compa.h5repack_layout.h5
         ./testfiles/out-chunk_conti.h5repack_layout.h5
         ./testfiles/out-compa.h5repack_layout.h5
         ./testfiles/out-conti.h5repack_layout.h5
         ./testfiles/out-deflate_file.h5repack_layout.h5
         ./testfiles/out-deflate_limit.h5repack_layout.h5
         ./testfiles/out-dset2_chunk_20x10.h5repack_layout.h5
         ./testfiles/out-dset2_compa.h5repack_layout.h5
         ./testfiles/out-dset2_conti.h5repack_layout.h5
         ./testfiles/out-dset_compa_chunk.h5repack_layout.h5
         ./testfiles/out-dset_compa_compa.h5repack_layout.h5
         ./testfiles/out-dset_compa_conti.h5repack_layout.h5
         ./testfiles/out-dset_conti_chunk.h5repack_layout.h5
         ./testfiles/out-dset_conti_compa.h5repack_layout.h5
         ./testfiles/out-dset_conti_conti.h5repack_layout.h5
         ./testfiles/out-fletcher_all.h5repack_layout.h5
         ./testfiles/out-fletcher_individual.h5repack_layout.h5
         ./testfiles/out-global_filters.h5repack_layout.h5
         ./testfiles/out-gzip_all.h5repack_layout.h5
         ./testfiles/out-gzip_individual.h5repack_layout.h5
         ./testfiles/out-layout.h5repack_layout.h5
         ./testfiles/out-layout_long_switches.h5repack_layout.h5
         ./testfiles/out-layout_short_switches.h5repack_layout.h5
         ./testfiles/out-old_style_layout_short_switches.h5repack_layout.h5
         ./testfiles/out-plugin_test.h5repack_layout.h5
         ./testfiles/out-shuffle_all.h5repack_layout.h5
         ./testfiles/out-shuffle_individual.h5repack_layout.h5
         ./testfiles/out-upgrade_layout.h5repack_layouto.h5
         ./testfiles/out-contig_small_compa.h5repack_layout2.h5
         ./testfiles/out-contig_small_fixed_compa.h5repack_layout2.h5
         ./testfiles/out-ckdim_biger.h5repack_layout3.h5
         ./testfiles/out-ckdim_smaller.h5repack_layout3.h5
         ./testfiles/out-chunk2chunk.h5repack_layout3.h5
         ./testfiles/out-chunk2compa.h5repack_layout3.h5
         ./testfiles/out-chunk2conti.h5repack_layout3.h5
         ./testfiles/out-error1.h5repack_layout3.h5
         ./testfiles/out-error2.h5repack_layout3.h5
         ./testfiles/out-error3.h5repack_layout3.h5
         ./testfiles/out-error4.h5repack_layout3.h5
         ./testfiles/out-committed_dt.h5repack_named_dtypes.h5
         ./testfiles/out-nbit_add.h5repack_nbit.h5
         ./testfiles/out-nbit_copy.h5repack_nbit.h5
         ./testfiles/out-nbit_remove.h5repack_nbit.h5
         ./testfiles/out-add_alignment.h5repack_objs.h5
         ./testfiles/out-add_userblock.h5repack_objs.h5
         ./testfiles/out-objs.h5repack_objs.h5
         ./testfiles/out-gt_mallocsize.h5repack_objs.h5
         ./testfiles/out-bug1814.h5repack_refs.h5
         ./testfiles/out-shuffle_copy.h5repack_shuffle.h5
         ./testfiles/out-shuffle_remove.h5repack_shuffle.h5
         ./testfiles/out-scale_add.h5repack_soffset.h5
         ./testfiles/out-scale_copy.h5repack_soffset.h5
         ./testfiles/out-scale_remove.h5repack_soffset.h5
         ./testfiles/out-meta_short_M.meta_short.h5
         ./testfiles/out-meta_short_N.meta_short.h5
         ./testfiles/out-meta_long_M.meta_long.h5
         ./testfiles/out-meta_long_N.meta_long.h5
         # from the h5repacktst
         h5repack_attr.h5
         h5repack_attr_out.h5
         h5repack_attr_refs.h5
         h5repack_big.h5
         h5repack_deflate.h5 
         h5repack_deflate_out.h5
         h5repack_early2.h5
         h5repack_early.h5
         h5repack_early_out.h5
         h5repack_ext.h5
         h5repack_ext_out.h5
         h5repack_fill.h5
         h5repack_fill_out.h5
         h5repack_filters.h5
         h5repack_filters_out.h5
         h5repack_fletcher.h5
         h5repack_fletcher_out.h5
         h5repack_hlink.h5
         h5repack_hlink_out.h5
         h5repack_layout.h5
         h5repack_layout_out.h5
         h5repack_layout2.h5
         h5repack_layout3.h5
         h5repack_named_dtypes.h5
         h5repack_named_dtypes_out.h5
         h5repack_nbit.h5
         h5repack_nbit_out.h5
         h5repack_objs.h5
         h5repack_objs_out.h5
         h5repack_refs.h5
         h5repack_shuffle.h5
         h5repack_shuffle_out.h5
         h5repack_soffset.h5 
         h5repack_soffset_out.h5
         h5repack_szip.h5
         h5repack_szip_out.h5
         h5repack_ub.h5
         h5repack_ub_out.h5
         h5repack_ext.bin
         ublock.bin
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5REPACK-clearall-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")

  ADD_HELP_TEST(help 0 -h)

  ADD_TEST (NAME H5REPACK-testh5repack_detect_szip COMMAND $<TARGET_FILE:testh5repack_detect_szip>)
  IF (HDF5_ENABLE_SZIP_SUPPORT)
    IF (HDF5_ENABLE_SZIP_ENCODING)
      SET (passRegex "yes")
      SET_TESTS_PROPERTIES (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "yes")
    ELSE (HDF5_ENABLE_SZIP_ENCODING)
      SET (passRegex "no")
      SET_TESTS_PROPERTIES (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
    ENDIF (HDF5_ENABLE_SZIP_ENCODING)
  ELSE (HDF5_ENABLE_SZIP_SUPPORT)
    SET (passRegex "no")
    SET_TESTS_PROPERTIES (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
  ENDIF (HDF5_ENABLE_SZIP_SUPPORT)
  SET_TESTS_PROPERTIES (H5REPACK-testh5repack_detect_szip PROPERTIES DEPENDS H5REPACK-clearall-objects)

  ADD_TEST (NAME H5REPACK-h5repacktest COMMAND $<TARGET_FILE:h5repacktest>)
  SET_TESTS_PROPERTIES (H5REPACK-h5repacktest PROPERTIES DEPENDS H5REPACK-testh5repack_detect_szip)
  SET (last_test "H5REPACK-h5repacktest")

#
# The tests
# We use the files generated by h5repacktst
# Each run generates "<file>.out.h5" and the tool h5diff is used to
# compare the input and output files
#
# the tests are the same as the program h5repacktst, but run from the CLI 
#

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present. 
  SET (USE_FILTER_SZIP_ENCODER "no")
  IF (HDF5_ENABLE_SZIP_ENCODING)
    SET (USE_FILTER_SZIP_ENCODER ${testh5repack_detect_szip})
  ENDIF (HDF5_ENABLE_SZIP_ENCODING)

  IF (H5_HAVE_FILTER_DEFLATE)
    SET (USE_FILTER_DEFLATE "true")
  ENDIF (H5_HAVE_FILTER_DEFLATE)

  IF (H5_HAVE_FILTER_SZIP)
    SET (USE_FILTER_SZIP "true")
  ENDIF (H5_HAVE_FILTER_SZIP)

  IF (H5_HAVE_FILTER_SHUFFLE)
    SET (USE_FILTER_SHUFFLE "true")
  ENDIF (H5_HAVE_FILTER_SHUFFLE)

  IF (H5_HAVE_FILTER_FLETCHER32)
    SET (USE_FILTER_FLETCHER32 "true")
  ENDIF (H5_HAVE_FILTER_FLETCHER32)

  IF (H5_HAVE_FILTER_NBIT)
    SET (USE_FILTER_NBIT "true")
  ENDIF (H5_HAVE_FILTER_NBIT)

  IF (H5_HAVE_FILTER_SCALEOFFSET)
    SET (USE_FILTER_SCALEOFFSET "true")
  ENDIF (H5_HAVE_FILTER_SCALEOFFSET)
  
# copy files (these files have no filters) 
  ADD_H5_TEST (fill "TEST" ${FILE0})
  ADD_H5_TEST (objs "TEST" ${FILE1})
  ADD_H5_TEST (attr "TEST" ${FILE2})
  ADD_H5_TEST (hlink "TEST" ${FILE3})
  ADD_H5_TEST (layout "TEST" ${FILE4})
  ADD_H5_TEST (early "TEST" ${FILE5})

# use $FILE4 to write some filters  (this file has  no filters)

# gzip with individual object
  SET (arg ${FILE4} -f dset1:GZIP=1  -l dset1:CHUNK=20x10)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (gzip_individual ${TESTTYPE} ${arg})
  
# gzip for all 
  SET (arg ${FILE4} -f GZIP=1)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (gzip_all ${TESTTYPE} ${arg})

# szip with individual object
  SET (arg ${FILE4} -f dset2:SZIP=8,EC  -l dset2:CHUNK=20x10)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  ADD_H5_TEST (szip_individual ${TESTTYPE} ${arg}) 

# szip for all
  SET (arg ${FILE4} -f SZIP=8,NN)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  ADD_H5_TEST (szip_all ${TESTTYPE} ${arg}) 

# shuffle with individual object
  SET (arg ${FILE4} -f dset2:SHUF  -l dset2:CHUNK=20x10)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SHUFFLE)
  ADD_H5_TEST (shuffle_individual ${TESTTYPE} ${arg}) 

# shuffle for all
  SET (arg ${FILE4} -f SHUF)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SHUFFLE)
  ADD_H5_TEST (shuffle_all ${TESTTYPE} ${arg})
  
# fletcher32  with individual object
  SET (arg ${FILE4} -f dset2:FLET  -l dset2:CHUNK=20x10)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32)
  ADD_H5_TEST (fletcher_individual ${TESTTYPE} ${arg})

# fletcher32 for all
  SET (arg ${FILE4} -f FLET)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32)
  ADD_H5_TEST (fletcher_all ${TESTTYPE} ${arg})

# all filters
  SET (arg ${FILE4} -f dset2:SHUF -f dset2:FLET -f dset2:SZIP=8,NN -f dset2:GZIP=1 -l dset2:CHUNK=20x10)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (all_filters ${TESTTYPE} ${arg})

# verbose gzip with individual object
  SET (arg ${FILE11} -v -f /dset_deflate:GZIP=9)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_CMP_TEST (gzip_verbose_filters "O?...ing file[^\n]+\n" ${TESTTYPE} 0 ${arg})
  
###########################################################
# the following tests assume the input files have filters
###########################################################

# szip copy
  SET (arg ${FILE7})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  ADD_H5_TEST (szip_copy ${TESTTYPE} ${arg})
  
# szip remove
  SET (arg ${FILE7} --filter=dset_szip:NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP)
  ADD_H5_TEST (szip_remove ${TESTTYPE} ${arg})
  
# deflate copy
  SET (arg ${FILE8})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (deflate_copy ${TESTTYPE} ${arg})

# deflate remove
  SET (arg ${FILE8} -f dset_deflate:NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (deflate_remove ${TESTTYPE} ${arg})
    
# shuffle copy
  SET (arg ${FILE9})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SHUFFLE)
  ADD_H5_TEST (shuffle_copy ${TESTTYPE} ${arg})

# shuffle remove
  SET (arg ${FILE9} -f dset_shuffle:NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SHUFFLE)
  ADD_H5_TEST (shuffle_remove ${TESTTYPE} ${arg})

# fletcher32 copy
  SET (arg ${FILE10})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32)
  ADD_H5_TEST (fletcher_copy ${TESTTYPE} ${arg})

# fletcher32 remove
  SET (arg ${FILE10} -f dset_fletcher32:NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32)
  ADD_H5_TEST (fletcher_remove ${TESTTYPE} ${arg})

# nbit copy
  SET (arg ${FILE12})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_NBIT)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_NBIT)
  ADD_H5_TEST (nbit_copy ${TESTTYPE} ${arg})

# nbit remove
  SET (arg ${FILE12} -f dset_nbit:NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_NBIT)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_NBIT)
  ADD_H5_TEST (nbit_remove ${TESTTYPE} ${arg})

# nbit add
  SET (arg ${FILE12} -f dset_int31:NBIT)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_NBIT)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_NBIT)
  ADD_H5_TEST (nbit_add ${TESTTYPE} ${arg})

# scaleoffset copy
  SET (arg ${FILE13})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SCALEOFFSET)
  ADD_H5_TEST (scale_copy ${TESTTYPE} ${arg})

# scaleoffset add
  SET (arg ${FILE13} -f dset_none:SOFF=31,IN)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SCALEOFFSET)
  ADD_H5_TEST (scale_add ${TESTTYPE} ${arg})

# scaleoffset remove
  SET (arg ${FILE13} -f dset_scaleoffset:NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SCALEOFFSET)
  ADD_H5_TEST (scale_remove ${TESTTYPE} ${arg})

# remove all  filters
  SET (arg ${FILE11} -f NONE)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_FLETCHER32 OR NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SHUFFLE OR NOT USE_FILTER_NBIT OR NOT USE_FILTER_SCALEOFFSET)
  ADD_H5_TEST (remove_all ${TESTTYPE} ${arg})

#filter conversions
  SET (arg ${FILE8} -f dset_deflate:SZIP=8,NN)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (deflate_convert ${TESTTYPE} ${arg}) 

  SET (arg ${FILE7} -f dset_szip:GZIP=1)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_SZIP OR NOT USE_FILTER_SZIP_ENCODER OR NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (szip_convert ${TESTTYPE} ${arg}) 

#limit
  SET (arg ${FILE4} -f GZIP=1 -m 1024)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_DMP_TEST (deflate_limit ${TESTTYPE} 0 ${arg})

#file
  SET (arg ${FILE4} -e ${INFO_FILE})
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST (deflate_file ${TESTTYPE} ${arg}) 

#########################################################
# layout options (these files have no filters)
#########################################################
  ADD_H5_VERIFY_TEST (dset2_chunk_20x10 "TEST" 0 ${FILE4} dset2 CHUNKED --layout=dset2:CHUNK=20x10)
  ADD_H5_VERIFY_TEST (chunk_20x10 "TEST" 1 ${FILE4} null CHUNKED -l CHUNK=20x10)
  ADD_H5_VERIFY_TEST (dset2_conti "TEST" 0 ${FILE4} dset2 CONTIGUOUS -l dset2:CONTI)
  ADD_H5_VERIFY_TEST (conti "TEST" 1 ${FILE4} null CONTIGUOUS -l CONTI)
  ADD_H5_VERIFY_TEST (dset2_compa "TEST" 0 ${FILE4} dset2 COMPACT -l dset2:COMPA)
  ADD_H5_VERIFY_TEST (compa "TEST" 1 ${FILE4} null COMPACT -l COMPA)

################################################################
# layout conversions (file has no filters)
###############################################################
  ADD_H5_VERIFY_TEST (dset_compa_conti "TEST" 0 ${FILE4} dset_compact CONTIGUOUS -l dset_compact:CONTI)
  ADD_H5_VERIFY_TEST (dset_compa_chunk "TEST" 0 ${FILE4} dset_compact CHUNKED -l dset_compact:CHUNK=2x5)
  ADD_H5_VERIFY_TEST (dset_compa_compa "TEST" 0 ${FILE4} dset_compact COMPACT -l dset_compact:COMPA)
  ADD_H5_VERIFY_TEST (dset_conti_compa "TEST" 0 ${FILE4} dset_contiguous COMPACT -l dset_contiguous:COMPA)
  ADD_H5_VERIFY_TEST (dset_conti_chunk "TEST" 0 ${FILE4} dset_contiguous CHUNKED -l dset_contiguous:CHUNK=3x6)
  ADD_H5_VERIFY_TEST (dset_conti_conti "TEST" 0 ${FILE4} dset_contiguous CONTIGUOUS -l dset_contiguous:CONTI)
  ADD_H5_VERIFY_TEST (chunk_compa "TEST" 0 ${FILE4} dset_chunk COMPACT -l dset_chunk:COMPA)
  ADD_H5_VERIFY_TEST (chunk_conti "TEST" 0 ${FILE4} dset_chunk CONTIGUOUS -l dset_chunk:CONTI)
  ADD_H5_VERIFY_TEST (chunk_18x13 "TEST" 0 ${FILE4} dset_chunk CHUNKED -l dset_chunk:CHUNK=18x13)

# test convert small size dataset ( < 1k) to compact layout without -m
  ADD_H5_VERIFY_TEST (contig_small_compa "TEST" 0 ${FILE18} contig_small COMPACT -l contig_small:COMPA)
  ADD_H5_VERIFY_TEST (contig_small_fixed_compa "TEST" 0 ${FILE18} chunked_small_fixed COMPACT -l chunked_small_fixed:COMPA)

#--------------------------------------------------------------------------- 
# Test file contains chunked datasets (need multiple dsets) with 
# unlimited max dims.   (HDFFV-7933)
# Use first dset to test.
#---------------------------------------------------------------------------
# chunk to chunk - specify chunk dim bigger than any current dim
ADD_H5_VERIFY_TEST (chunk2chunk "TEST" 0 h5repack_layout3.h5 chunk_unlimit1 CHUNK -l chunk_unlimit1:CHUNK=100x300)

# chunk to contiguous 
ADD_H5_VERIFY_TEST (chunk2conti "TEST" 0 h5repack_layout3.h5 chunk_unlimit1 CONTI -l chunk_unlimit1:CONTI)

# chunk to compact - convert big dataset (should be > 64k) for this purpose, 
# should remain as original layout (chunk)
ADD_H5_VERIFY_TEST (chunk2compa "TEST" 0 h5repack_layout3.h5 chunk_unlimit1 CHUNK -l chunk_unlimit1:COMPA)

#--------------------------------------------------------------------------
# Test -f for some specific cases. Chunked dataset with unlimited max dims.
# (HDFFV-8012)
#--------------------------------------------------------------------------
# - should not fail 
# - should not change max dims from unlimit

# chunk dim is bigger than dataset dim. ( dset size < 64k )
ADD_H5_VERIFY_TEST (error1 "TEST" 0 h5repack_layout3.h5 chunk_unlimit1 H5S_UNLIMITED -f chunk_unlimit1:NONE)

# chunk dim is bigger than dataset dim. ( dset size > 64k )
ADD_H5_VERIFY_TEST (error2 "TEST" 0 h5repack_layout3.h5 chunk_unlimit2 H5S_UNLIMITED -f chunk_unlimit2:NONE)

# chunk dims are smaller than dataset dims. ( dset size < 64k )
ADD_H5_VERIFY_TEST (error3 "TEST" 0 h5repack_layout3.h5 chunk_unlimit3 H5S_UNLIMITED -f chunk_unlimit3:NONE)

# file input - should not fail
ADD_H5_TEST (error4 "TEST" h5repack_layout3.h5 -f NONE)

#--------------------------------------------------------------------------
# Test base: Convert CHUNK to CONTI for a chunked dataset with small dataset 
# (dset size < 64K) and with unlimited max dims on a condition as follow.
# (HDFFV-8214)
#--------------------------------------------------------------------------
# chunk dim is bigger than dataset dim. should succeed.
ADD_H5_VERIFY_TEST (ckdim_biger "TEST" 0 h5repack_layout3.h5 chunk_unlimit2 CONTI -l chunk_unlimit2:CONTI)
# chunk dim is smaller than dataset dim. should succeed.  
ADD_H5_VERIFY_TEST (ckdim_smaller "TEST" 0 h5repack_layout3.h5 chunk_unlimit3 CONTI -l chunk_unlimit3:CONTI)



# Native option
# Do not use FILE1, as the named dtype will be converted to native, and h5diff will
# report a difference.
  ADD_H5_TEST (native_fill "TEST" ${FILE0} -n)
  ADD_H5_TEST (native_attr "TEST" ${FILE2} -n)

# latest file format with long switches. use FILE4=h5repack_layout.h5 (no filters)
  SET (arg --layout CHUNK=20x10 --filter GZIP=1 --minimum=10 --native --latest --compact=8 --indexed=6 --ssize=8[:dtype])
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_VERIFY_TEST (layout_long_switches ${TESTTYPE} 1 ${FILE4} null CHUNKED ${arg})

# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
  SET (arg -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_VERIFY_TEST (layout_short_switches ${TESTTYPE} 1 ${FILE4} null CHUNKED ${arg})

# several global filters
  SET (arg ${FILE4} --filter GZIP=1 --filter SHUF)
  SET (TESTTYPE "TEST")
  IF (NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SHUFFLE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE OR NOT USE_FILTER_SHUFFLE)
  ADD_H5_TEST (global_filters ${TESTTYPE} ${arg})

# syntax of -i infile -o outfile
# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
  SET (arg ${FILE4} -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
  SET (TESTTYPE "LEGACY")
  IF (NOT USE_FILTER_DEFLATE)
    SET (TESTTYPE "SKIP")
  ENDIF (NOT USE_FILTER_DEFLATE)
  ADD_H5_TEST_OLD (old_style_layout_short_switches ${TESTTYPE} ${arg})

# add a userblock to file
  SET (arg ${FILE1} -u ${PROJECT_BINARY_DIR}/testfiles/ublock.bin -b 2048)
  ADD_H5_TEST (add_userblock "TEST" ${arg})

# add alignment
  SET (arg ${FILE1} -t 1 -a 1)
  ADD_H5_TEST (add_alignment "TEST" ${arg})

# Check repacking file with old version of layout message (should get upgraded
# to new version and be readable, etc.)
  ADD_H5_TEST (upgrade_layout "TEST" ${FILE14})

# test for datum size > H5TOOLS_MALLOCSIZE
  ADD_H5_TEST (gt_mallocsize "TEST" ${FILE1} -f GZIP=1)

# Check repacking file with committed datatypes in odd configurations
  ADD_H5_TEST (committed_dt "TEST" ${FILE15})

# tests family driver (file is located in common testfiles folder, uses TOOLTEST1
  ADD_H5_TEST (family "TEST" ${FILE16})

# test various references (bug 1814 and 1726)
  ADD_H5_TEST (bug1814 "TEST" ${FILE_REF})

# test attribute with various references (bug1797 / HDFFV-5932)
# the references in attribute of compund or vlen datatype 
  ADD_H5_TEST (HDFFV-5932 "TEST" ${FILE_ATTR_REF})

# Add test for memory leak in attirbute. This test is verified by CTEST.   
# 1. leak from vlen string  
# 2. leak from compound type without reference member
# (HDFFV-7840, )
# Note: this test is experimental for sharing test file among tools
  ADD_H5_TEST (HDFFV-7840 "TEST" h5diff_attr1.h5)

# tests for metadata block size option ('-M')
  ADD_H5_TEST_META (meta_short h5repack_layout.h5 -M 8192)
  ADD_H5_TEST_META (meta_long h5repack_layout.h5 --metadata_block_size=8192)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
IF (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (plugin_test 0 h5repack_layout.h5 -v -f UD=257,1,9)
#  ADD_H5_UD_TEST (plugin_none 0 h5repack_layout.UD.h5 -v -f NONE)
ELSE (BUILD_SHARED_LIBS)
  MESSAGE (STATUS " **** Plugins libraries must be built as shared libraries **** ")
  ADD_TEST (
      NAME H5REPACK-plugin
      COMMAND ${CMAKE_COMMAND} -E echo "SKIP H5PLUGIN TESTING"
  )
ENDIF (BUILD_SHARED_LIBS)

  IF (HDF5_TEST_VFD)
    # Run test with different Virtual File Driver
    FOREACH (vfd ${VFD_LIST})
      ADD_VFD_TEST (${vfd} 0)
    ENDFOREACH (vfd ${VFD_LIST})
  ENDIF (HDF5_TEST_VFD)
