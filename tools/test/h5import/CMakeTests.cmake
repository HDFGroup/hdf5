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

  set (HDF5_REFERENCE_CONF_FILES
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
  set (HDF5_REFERENCE_TXT_FILES
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
      tall_fp32.ddl
      tall_i32.ddl
      tintsattrs_u32.ddl
  )
  set (HDF5_REFERENCE_TEST_FILES
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
  set (HDF5_TOOLS_TEST_FILES
      tall.h5
      tintsattrs.h5
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  foreach (conf_file ${HDF5_REFERENCE_CONF_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5IMPORT_SOURCE_DIR}/testfiles/${conf_file}" "${PROJECT_BINARY_DIR}/testfiles/${conf_file}" "h5import_files")
  endforeach ()

  foreach (txt_file ${HDF5_REFERENCE_TXT_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5IMPORT_SOURCE_DIR}/testfiles/${txt_file}" "${PROJECT_BINARY_DIR}/testfiles/${txt_file}" "h5import_files")
  endforeach ()

  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5IMPORT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5import_files")
  endforeach ()

  foreach (h5_file ${HDF5_TOOLS_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5import_files")
  endforeach ()
  add_custom_target(h5import_files ALL COMMENT "Copying files needed by h5import tests" DEPENDS ${h5import_files_list})

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

  macro (ADD_H5_TEST testname importfile conffile testfile)
    # If using memchecker skip macro based tests
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5IMPORT-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5import${tgt_ext}> ${importfile} -c ${conffile} -o ${testfile})
      set_tests_properties (H5IMPORT-${testname} PROPERTIES
          FIXTURES_REQUIRED set_h5importtest
      )
    else ()
      add_test (
          NAME H5IMPORT-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${testfile}
      )
      set_tests_properties (H5IMPORT-${testname}-clear-objects PROPERTIES
          FIXTURES_REQUIRED set_h5importtest
      )

      add_test (NAME H5IMPORT-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5import${tgt_ext}> ${importfile} -c ${conffile} -o ${testfile})
      set_tests_properties (H5IMPORT-${testname} PROPERTIES
          DEPENDS H5IMPORT-${testname}-clear-objects
      )

      add_test (
          NAME H5IMPORT-${testname}-H5DMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testfile}.new"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT-${testname}-H5DMP PROPERTIES
          DEPENDS H5IMPORT-${testname}
      )
      add_test (
          NAME H5IMPORT-${testname}-H5DMP_CMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=testfiles/${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testfile}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_REFERENCE=${testfile}.new"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT-${testname}-H5DMP_CMP PROPERTIES
          DEPENDS H5IMPORT-${testname}-H5DMP
      )
    endif ()
  endmacro ()

  macro (ADD_H5_DUMPTEST testname datasetname testfile)
    # If using memchecker skip tests
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5IMPORT-DUMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              d${testfile}
              d${testfile}.bin
      )
      set_tests_properties (H5IMPORT-DUMP-${testname}-clear-objects PROPERTIES
          DEPENDS H5IMPORT-${testname}-H5DMP_CMP
      )

      if ("${ARGN}" STREQUAL "BINARY")
        add_test (
            NAME H5IMPORT-DUMP-${testname}-H5DMP
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
                -D "TEST_ARGS:STRING=-p;-d;${datasetname};-o;d${testfile}.bin;-b;NATIVE;testfiles/${testfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=d${testfile}.dmp"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME H5IMPORT-DUMP-${testname}-H5DMP
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
                -D "TEST_ARGS:STRING=-p;-d;${datasetname};-o;d${testfile}.bin;-y;--width=1;testfiles/${testfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=d${testfile}.dmp"
                -D "TEST_EXPECT=0"
                -D "TEST_SKIP_COMPARE=TRUE"
                -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5IMPORT-DUMP-${testname}-H5DMP PROPERTIES
          DEPENDS "H5IMPORT-DUMP-${testname}-clear-objects"
      )

      add_test (
          NAME H5IMPORT-DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5import${tgt_ext}>"
              -D "TEST_ARGS:STRING=d${testfile}.bin;-c;d${testfile}.dmp;-o;d${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=d${testfile}.imp"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT-DUMP-${testname} PROPERTIES
          DEPENDS "H5IMPORT-DUMP-${testname}-H5DMP"
      )

      add_test (
          NAME H5IMPORT-DUMP-${testname}-H5DFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff${tgt_ext}>"
              -D "TEST_ARGS:STRING=-r;d${testfile};testfiles/${testfile};${datasetname};${datasetname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=d${testfile}.dff"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(Warning)[^\n]*)"
              -D "TEST_REFERENCE=testfiles/d${testfile}.txt"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT-DUMP-${testname}-H5DFF PROPERTIES
          DEPENDS "H5IMPORT-DUMP-${testname}"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_DUMPSUBTEST testname testfile datasetname)
    # If using memchecker skip tests
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5IMPORT_SUB-DUMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              d-${testname}.h5
              ${testname}.bin
      )
      set_tests_properties (H5IMPORT_SUB-DUMP-${testname}-clear-objects PROPERTIES
          FIXTURES_REQUIRED set_h5importtest
      )

      add_test (
          NAME H5IMPORT_SUB-DUMP-${testname}-H5DMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=-p;-d;${datasetname};${ARGN};-o;${testname}.bin;-b;NATIVE;testfiles/${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.dmp"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT_SUB-DUMP-${testname}-H5DMP PROPERTIES
          DEPENDS "H5IMPORT_SUB-DUMP-${testname}-clear-objects"
      )

      add_test (
          NAME H5IMPORT_SUB-DUMP-${testname}-H5IMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5import${tgt_ext}>"
              -D "TEST_ARGS:STRING=${testname}.bin;-c;${testname}.dmp;-o;d-${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${testname}.imp"
              -D "TEST_EXPECT=0"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT_SUB-DUMP-${testname}-H5IMP PROPERTIES
          DEPENDS "H5IMPORT_SUB-DUMP-${testname}-H5DMP"
      )
      add_test (
          NAME H5IMPORT_SUB-DUMP-${testname}-CMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=-p;d-${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=d-${testname}.dmp"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=testfiles/${testname}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5IMPORT_SUB-DUMP-${testname}-CMP PROPERTIES
         DEPENDS "H5IMPORT_SUB-DUMP-${testname}-H5IMP"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_SKIP_DUMPTEST testname datasetname testfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5IMPORT-DUMP-${testname}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${testname} ${datasetname} ${testfile} --- DEFLATE filter not available"
      )
      set_property(TEST H5IMPORT-DUMP-${testname} PROPERTY DISABLED)
    endif ()
  endmacro ()

  # --------------------------------------------------------------------
  # Determine if filter is available for h5diff
  # --------------------------------------------------------------------
  if (H5_HAVE_FILTER_DEFLATE)
    set (USE_FILTER_DEFLATE "true")
  endif ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5IMPORT-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            binfp64.bin
            binin8.bin
            binin8w.bin
            binin16.bin
            binin32.bin
            binuin16.bin
            binuin32.bin
            txtin32.h5
            txtin16.h5
            txtin8.h5
            txtuin16.h5
            txtuin32.h5
            txtfp32.h5
            txtfp64.h5
            binfp64.h5
            binin8.h5
            binin8w.h5
            binin16.h5
            binin32.h5
            binuin16.h5
            binuin32.h5
            txtstr.h5
            textpfe.h5
            dbinfp64.h5
            dbinfp64.h5.bin
            dbinin8.h5
            dbinin8.h5.bin
            dbinin8w.h5
            dbinin8w.h5.bin
            dbinin16.h5
            dbinin16.h5.bin
            dbinin32.h5
            dbinin32.h5.bin
            dbinuin16.h5
            dbinuin16.h5.bin
            dbinuin32.h5
            dbinuin32.h5.bin
            dtxtstr.h5
            dtxtstr.h5.bin
            tall_fp32.bin
            d-tall_fp32.h5
            tall_i32.bin
            d-tall_i32.h5
    )
    set (last_test "H5IMPORT-clear-objects")
  endif ()

  add_test (
      NAME H5IMPORT-h5importtest-clear-objects
      COMMAND ${CMAKE_COMMAND} -E remove
          binfp64.bin
          binfp64.conf
          binin8.bin
          binin8.conf
          binin8w.bin
          binin8w.conf
          binin16.bin
          binin16.conf
          binin32.bin
          binin32.conf
          binuin16.bin
          binuin16.conf
          binuin32.bin
          binuin32.conf
  )

  add_test (NAME H5IMPORT-h5importtest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5importtest>)
  set_tests_properties (H5IMPORT-h5importtest PROPERTIES
      FIXTURES_SETUP set_h5importtest
      DEPENDS H5IMPORT-h5importtest-clear-objects
  )

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
  ADD_H5_TEST (BINARY_F64 binfp64.bin binfp64.conf binfp64.h5)
  if (NOT USE_FILTER_DEFLATE)
    ADD_H5_SKIP_DUMPTEST (BINARY_F64 "/fp/bin/64-bit" binfp64.h5 BINARY)
  else ()
    ADD_H5_DUMPTEST (BINARY_F64 "/fp/bin/64-bit" binfp64.h5 BINARY)
  endif ()

  # ----- TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed "
  ADD_H5_TEST (BINARY_I8 binin8.bin binin8.conf binin8.h5)
  if (NOT USE_FILTER_DEFLATE)
    ADD_H5_SKIP_DUMPTEST (BINARY_I8 "/int/bin/8-bit" binin8.h5 BINARY)
  else ()
    ADD_H5_DUMPTEST (BINARY_I8 "/int/bin/8-bit" binin8.h5 BINARY)
  endif ()

  # ----- TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended "
  ADD_H5_TEST (BINARY_I16 binin16.bin binin16.conf binin16.h5)
  ADD_H5_DUMPTEST (BINARY_I16 "/int/bin/16-bit" binin16.h5 BINARY)

  # ----- TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED "
  ADD_H5_TEST (BINARY_I32 binin32.bin binin32.conf binin32.h5)
  ADD_H5_DUMPTEST (BINARY_I32 "/int/bin/32-bit" binin32.h5 BINARY)

  # ----- TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED "
  ADD_H5_TEST (BINARY_UI16 binuin16.bin binuin16.conf binuin16.h5)
  ADD_H5_DUMPTEST (BINARY_UI16 "/int/buin/16-bit" binuin16.h5 BINARY)

  # ----- TESTING "BINARY UI32 - rank 3 - Output LE "
  ADD_H5_TEST (BINARY_UI32 binuin32.bin binuin32.conf binuin32.h5)
  ADD_H5_DUMPTEST (BINARY_UI32 "/int/buin/32-bit" binuin32.h5 BINARY)

  # ----- TESTING "STR"
  ADD_H5_TEST (STR testfiles/txtstr.txt testfiles/txtstr.conf txtstr.h5)
  ADD_H5_DUMPTEST (STR "/mytext/data" txtstr.h5)

  # ----- TESTING "BINARY I8 CR LF EOF"
  ADD_H5_TEST (BINARY_I8_EOF binin8w.bin binin8w.conf binin8w.h5)
  ADD_H5_DUMPTEST (BINARY_I8_EOF "/dataset0" binin8w.h5 BINARY)

  # ----- TESTING "ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE "
  ADD_H5_TEST (ASCII_F64_R1 testfiles/textpfe64.txt testfiles/textpfe.conf textpfe.h5)

  # ----- TESTING "Binary Subset "
  ADD_H5_DUMPSUBTEST (tall_fp32 tall.h5 /g2/dset2.2 --start=1,1 --stride=2,3 --count=1,2 --block=1,1)
  ADD_H5_DUMPSUBTEST (tall_i32 tall.h5 /g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1)
  ADD_H5_DUMPSUBTEST (tintsattrs_u32 tintsattrs.h5 /DU32BITS --start=1,1 --stride=2,3 --count=3,2 --block=1,1)

