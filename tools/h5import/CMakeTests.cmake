
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  SET (HDF5_REFERENCE_CONF_FILES
      binfp64.conf
      binin8.conf
      binin8w.conf
      binin16.conf
      binin32.conf
      binuin16.conf
      binuin32.conf
      txtfp32.conf
      txtfp64.conf
      txtin8.conf
      txtin16.conf
      txtin32.conf
      txtuin16.conf
      txtuin32.conf
      textpfe.conf
      txtstr.conf
  )
  SET (HDF5_REFERENCE_TXT_FILES
      txtfp32.txt
      txtfp64.txt
      txtuin16.txt
      txtuin32.txt
      txtin8.txt
      txtin16.txt
      txtin32.txt
      textpfe64.txt
      txtstr.txt
      dbinfp64.h5.txt
      dbinin8.h5.txt
      dbinin8w.h5.txt
      dbinin16.h5.txt
      dbinin32.h5.txt
      dbinuin16.h5.txt
      dbinuin32.h5.txt
      dtxtstr.h5.txt
  )
  SET (HDF5_REFERENCE_TEST_FILES
      binfp64.h5
      binin8.h5
      binin8w.h5
      binin16.h5
      binin32.h5
      binuin16.h5
      binuin32.h5
      txtfp32.h5
      txtfp64.h5
      txtin8.h5
      txtin16.h5
      txtin32.h5
      txtuin16.h5
      txtuin32.h5
      txtstr.h5
      textpfe.h5
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  FOREACH (conf_file ${HDF5_REFERENCE_CONF_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${conf_file}")
    #MESSAGE (STATUS " Copying ${conf_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5import
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/testfiles/${conf_file} ${dest}
    )
  ENDFOREACH (conf_file ${HDF5_REFERENCE_CONF_FILES})

  FOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${txt_file}")
    #MESSAGE (STATUS " Copying ${txt_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5import
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/testfiles/${txt_file} ${dest}
    )
  ENDFOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5import
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################
  MACRO (ADD_H5_TEST testname importfile conffile testfile)
    # If using memchecker skip macro based tests
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5IMPORT-${testname} COMMAND $<TARGET_FILE:h5import> ${importfile} -c ${conffile} -o ${testfile})
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5IMPORT-${testname} PROPERTIES DEPENDS H5IMPORT-h5importtest)
      ENDIF (NOT "${last_test}" STREQUAL "")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5IMPORT-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${testfile}
              ${testfile}.new
              ${testfile}.new.err
              ${testfile}.out
              ${testfile}.out.err
      )
      SET_TESTS_PROPERTIES (H5IMPORT-${testname}-clear-objects PROPERTIES DEPENDS H5IMPORT-h5importtest)

      ADD_TEST (NAME H5IMPORT-${testname} COMMAND $<TARGET_FILE:h5import> ${importfile} -c ${conffile} -o ${testfile})
      SET_TESTS_PROPERTIES (H5IMPORT-${testname} PROPERTIES DEPENDS H5IMPORT-${testname}-clear-objects)

      ADD_TEST (
          NAME H5IMPORT-${testname}-H5DMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testfile}.new"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5IMPORT-${testname}-H5DMP PROPERTIES DEPENDS H5IMPORT-${testname})
      ADD_TEST (
          NAME H5IMPORT-${testname}-H5DMP_CMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=testfiles/${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testfile}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_REFERENCE=${testfile}.new"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5IMPORT-${testname}-H5DMP_CMP PROPERTIES DEPENDS H5IMPORT-${testname}-H5DMP)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST testname importfile conffile testfile)

  MACRO (ADD_H5_DUMPTEST testname datasetname testfile)
    # If using memchecker skip tests
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5IMPORT-DUMP-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              d${testfile}
              d${testfile}.bin
              d${testfile}.imp
              d${testfile}.imp.err
              d${testfile}.dmp
              d${testfile}.dmp.err
              d${testfile}.dff
              d${testfile}.dff.err
      )
      SET_TESTS_PROPERTIES (H5IMPORT-DUMP-${testname}-clear-objects PROPERTIES DEPENDS H5IMPORT-h5importtest)

      IF ("${ARGN}" STREQUAL "BINARY")
        ADD_TEST (
            NAME H5IMPORT-DUMP-${testname}-H5DMP
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=-p;-d;${datasetname};-o;d${testfile}.bin;-b;testfiles/${testfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=d${testfile}.dmp"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
        )
      ELSE ("${ARGN}" STREQUAL "BINARY")
        ADD_TEST (
            NAME H5IMPORT-DUMP-${testname}-H5DMP
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=-p;-d;${datasetname};-o;d${testfile}.bin;-y;--width=1;testfiles/${testfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=d${testfile}.dmp"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
        )
      ENDIF ("${ARGN}" STREQUAL "BINARY")
      SET_TESTS_PROPERTIES (H5IMPORT-DUMP-${testname}-H5DMP PROPERTIES DEPENDS "H5IMPORT-DUMP-${testname}-clear-objects")
      
      ADD_TEST (
          NAME H5IMPORT-DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5import>"
              -D "TEST_ARGS:STRING=d${testfile}.bin;-c;d${testfile}.dmp;-o;d${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=d${testfile}.imp"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5IMPORT-DUMP-${testname} PROPERTIES DEPENDS "H5IMPORT-DUMP-${testname}-H5DMP")

      ADD_TEST (
          NAME H5IMPORT-DUMP-${testname}-H5DFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;d${testfile};testfiles/${testfile};${datasetname};${datasetname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=d${testfile}.dff"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(Warning)[^\n]*)"
              -D "TEST_REFERENCE=testfiles/d${testfile}.txt"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5IMPORT-DUMP-${testname}-H5DFF PROPERTIES DEPENDS "H5IMPORT-DUMP-${testname}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_DUMPTEST testname datasetname testfile)

  MACRO (ADD_H5_SKIP_DUMPTEST testname datasetname testfile)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5IMPORT-DUMP-${testname}-SKIPPED
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${testname} ${datasetname} ${testfile} --- DEFLATE filter not available"
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_SKIP_DUMPTEST testname datasetname testfile)

  # --------------------------------------------------------------------
  # Determine if filter is available for h5diff
  # --------------------------------------------------------------------
  IF (H5_HAVE_FILTER_DEFLATE)
    SET (USE_FILTER_DEFLATE "true")
  ENDIF (H5_HAVE_FILTER_DEFLATE)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  IF (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    ADD_TEST (
        NAME H5IMPORT-clear-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove 
            binfp64.bin
            binin8.bin
            binin8w.bin
            binin16.bin
            binin32.bin
            binuin16.bin
            binuin32.bin
            txtin32.h5
            txtin32.h5.new
            txtin32.h5.new.err
            txtin32.h5.out
            txtin32.h5.out.err
            txtin16.h5
            txtin16.h5.new
            txtin16.h5.new.err
            txtin16.h5.out
            txtin16.h5.out.err
            txtin8.h5
            txtin8.h5.new
            txtin8.h5.new.err
            txtin8.h5.out
            txtin8.h5.out.err
            txtuin16.h5
            txtuin16.h5.new
            txtuin16.h5.new.err
            txtuin16.h5.out
            txtuin16.h5.out.err
            txtuin32.h5
            txtuin32.h5.new
            txtuin32.h5.new.err
            txtuin32.h5.out
            txtuin32.h5.out.err
            txtfp32.h5
            txtfp32.h5.new
            txtfp32.h5.new.err
            txtfp32.h5.out
            txtfp32.h5.out.err
            txtfp64.h5
            txtfp64.h5.new
            txtfp64.h5.new.err
            txtfp64.h5.out
            txtfp64.h5.out.err
            binfp64.h5
            binfp64.h5.new
            binfp64.h5.new.err
            binfp64.h5.out
            binfp64.h5.out.err
            binin8.h5
            binin8.h5.new
            binin8.h5.new.err
            binin8.h5.out
            binin8.h5.out.err
            binin8w.h5
            binin8w.h5.new
            binin8w.h5.new.err
            binin8w.h5.out
            binin8w.h5.out.err
            binin16.h5
            binin16.h5.new
            binin16.h5.new.err
            binin16.h5.out
            binin16.h5.out.err
            binin32.h5
            binin32.h5.new
            binin32.h5.new.err
            binin32.h5.out
            binin32.h5.out.err
            binuin16.h5
            binuin16.h5.new
            binuin16.h5.new.err
            binuin16.h5.out
            binuin16.h5.out.err
            binuin32.h5
            binuin32.h5.new
            binuin32.h5.new.err
            binuin32.h5.out
            binuin32.h5.out.err
            txtstr.h5
            txtstr.h5.new
            txtstr.h5.new.err
            txtstr.h5.out
            txtstr.h5.out.err
            textpfe.h5
            textpfe.h5.new
            textpfe.h5.new.err
            textpfe.h5.out
            textpfe.h5.out.err
            dbinfp64.h5
            dbinfp64.h5.bin
            dbinfp64.h5.imp
            dbinfp64.h5.imp.err
            dbinfp64.h5.dmp
            dbinfp64.h5.dmp.err
            dbinfp64.h5.dff
            dbinfp64.h5.dff.err
            dbinin8.h5
            dbinin8.h5.bin
            dbinin8.h5.imp
            dbinin8.h5.imp.err
            dbinin8.h5.dmp
            dbinin8.h5.dmp.err
            dbinin8.h5.dff
            dbinin8.h5.dff.err
            dbinin8w.h5
            dbinin8w.h5.bin
            dbinin8w.h5.imp
            dbinin8w.h5.imp.err
            dbinin8w.h5.dmp
            dbinin8w.h5.dmp.err
            dbinin8w.h5.dff
            dbinin8w.h5.dff.err
            dbinin16.h5
            dbinin16.h5.bin
            dbinin16.h5.imp
            dbinin16.h5.imp.err
            dbinin16.h5.dmp
            dbinin16.h5.dmp.err
            dbinin16.h5.dff
            dbinin16.h5.dff.err
            dbinin32.h5
            dbinin32.h5.bin
            dbinin32.h5.imp
            dbinin32.h5.imp.err
            dbinin32.h5.dmp
            dbinin32.h5.dmp.err
            dbinin32.h5.dff
            dbinin32.h5.dff.err
            dbinuin16.h5
            dbinuin16.h5.bin
            dbinuin16.h5.imp
            dbinuin16.h5.imp.err
            dbinuin16.h5.dmp
            dbinuin16.h5.dmp.err
            dbinuin16.h5.dff
            dbinuin16.h5.dff.err
            dbinuin32.h5
            dbinuin32.h5.bin
            dbinuin32.h5.imp
            dbinuin32.h5.imp.err
            dbinuin32.h5.dmp
            dbinuin32.h5.dmp.err
            dbinuin32.h5.dff
            dbinuin32.h5.dff.err
            dtxtstr.h5
            dtxtstr.h5.bin
            dtxtstr.h5.imp
            dtxtstr.h5.imp.err
            dtxtstr.h5.dmp
            dtxtstr.h5.dmp.err
            dtxtstr.h5.dff
            dtxtstr.h5.dff.err
    )
    SET (last_test "H5IMPORT-clear-objects")
  ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

  ADD_TEST (
      NAME H5IMPORT-h5importtest-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          binfp64.bin
          binin8.bin
          binin8w.bin
          binin16.bin
          binin32.bin
          binuin16.bin
          binuin32.bin
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5IMPORT-h5importtest-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5IMPORT-clear-objects")

  ADD_TEST (NAME H5IMPORT-h5importtest COMMAND $<TARGET_FILE:h5importtest>)
  SET_TESTS_PROPERTIES (H5IMPORT-h5importtest PROPERTIES DEPENDS H5IMPORT-h5importtest-clear-objects)

  # ----- TESTING "ASCII I32 rank 3 - Output BE " ;
  ADD_H5_TEST (ASCII_I32 testfiles/txtin32.txt testfiles/txtin32.conf txtin32.h5)

  # ----- TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
  ADD_H5_TEST (ASCII_I16 testfiles/txtin16.txt testfiles/txtin16.conf txtin16.h5)

  # ----- TESTING "ASCII I8 - rank 3 - Output I8 LE-Chunked+Extended+Compressed " 
  ADD_H5_TEST (ASCII_I8 testfiles/txtin8.txt testfiles/txtin8.conf txtin8.h5)

  # ----- TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
  ADD_H5_TEST (ASCII_UI16 testfiles/txtuin16.txt testfiles/txtuin16.conf txtuin16.h5)

  # ----- TESTING "ASCII UI32 - rank 3 - Output BE" 
  ADD_H5_TEST (ASCII_UI32 testfiles/txtuin32.txt testfiles/txtuin32.conf txtuin32.h5)

  # ----- TESTING "ASCII F32 - rank 3 - Output LE " 
  ADD_H5_TEST (ASCII_F32 testfiles/txtfp32.txt testfiles/txtfp32.conf txtfp32.h5)

  # ----- TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
  ADD_H5_TEST (ASCII_F64 testfiles/txtfp64.txt testfiles/txtfp64.conf txtfp64.h5)

  # ----- TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
  ADD_H5_TEST (BINARY_F64 binfp64.bin testfiles/binfp64.conf binfp64.h5)
  IF (NOT USE_FILTER_DEFLATE)
    ADD_H5_SKIP_DUMPTEST (BINARY_F64 "/fp/bin/64-bit" binfp64.h5 BINARY)
  ELSE (NOT USE_FILTER_DEFLATE)
    ADD_H5_DUMPTEST (BINARY_F64 "/fp/bin/64-bit" binfp64.h5 BINARY)
  ENDIF (NOT USE_FILTER_DEFLATE)

  # ----- TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
  ADD_H5_TEST (BINARY_I8 binin8.bin testfiles/binin8.conf binin8.h5)
  IF (NOT USE_FILTER_DEFLATE)
    ADD_H5_SKIP_DUMPTEST (BINARY_I8 "/int/bin/8-bit" binin8.h5 BINARY)
  ELSE (NOT USE_FILTER_DEFLATE)
    ADD_H5_DUMPTEST (BINARY_I8 "/int/bin/8-bit" binin8.h5 BINARY)
  ENDIF (NOT USE_FILTER_DEFLATE)

  # ----- TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
  ADD_H5_TEST (BINARY_I16 binin16.bin testfiles/binin16.conf binin16.h5)
  ADD_H5_DUMPTEST (BINARY_I16 "/int/bin/16-bit" binin16.h5 BINARY)

  # ----- TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
  ADD_H5_TEST (BINARY_I32 binin32.bin testfiles/binin32.conf binin32.h5)
  ADD_H5_DUMPTEST (BINARY_I32 "/int/bin/32-bit" binin32.h5 BINARY)

  # ----- TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
  ADD_H5_TEST (BINARY_UI16 binuin16.bin testfiles/binuin16.conf binuin16.h5)
  ADD_H5_DUMPTEST (BINARY_UI16 "/int/buin/16-bit" binuin16.h5 BINARY)

  # ----- TESTING "BINARY UI32 - rank 3 - Output LE " 
  ADD_H5_TEST (BINARY_UI32 binuin32.bin testfiles/binuin32.conf binuin32.h5)
  ADD_H5_DUMPTEST (BINARY_UI32 "/int/buin/32-bit" binuin32.h5 BINARY)

  # ----- TESTING "STR" 
  ADD_H5_TEST (STR testfiles/txtstr.txt testfiles/txtstr.conf txtstr.h5)
  ADD_H5_DUMPTEST (STR "/mytext/data" txtstr.h5)

  # ----- TESTING "BINARY I8 CR LF EOF" 
  ADD_H5_TEST (BINARY_I8_EOF binin8w.bin testfiles/binin8w.conf binin8w.h5)
  ADD_H5_DUMPTEST (BINARY_I8_EOF "/dataset0" binin8w.h5 BINARY)

  # ----- TESTING "ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE " 
  ADD_H5_TEST (ASCII_F64_R1 testfiles/textpfe64.txt testfiles/textpfe.conf textpfe.h5)

