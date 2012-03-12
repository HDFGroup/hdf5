cmake_minimum_required (VERSION 2.8.6)
PROJECT (HDF5_TOOLS_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5import ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5import.c)
TARGET_NAMING (h5import ${LIB_TYPE})
TARGET_LINK_LIBRARIES (h5import  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_TARGET_PROPERTIES (h5import PROPERTIES FOLDER tools)

SET (H5_DEP_EXECUTABLES h5import)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5import executables
  # --------------------------------------------------------------------
  ADD_EXECUTABLE (h5importtest ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5importtest.c)
  TARGET_NAMING (h5importtest ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  SET_TARGET_PROPERTIES (h5importtest PROPERTIES FOLDER tools)

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
      txtuin32.txt
      txtin16.txt
      txtin32.txt
      textpfe64.txt
      txtstr.txt
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
    SET (dest "${PROJECT_BINARY_DIR}/${txt_file}")
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
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
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
      IF (NOT "${last_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5IMPORT-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      ENDIF (NOT "${last_test}" STREQUAL "")
      SET (last_test "H5IMPORT-${testname}-clear-objects")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)

    ADD_TEST (NAME H5IMPORT-${testname} COMMAND $<TARGET_FILE:h5import> ${importfile} -c testfiles/${conffile} -o ${testfile})
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (H5IMPORT-${testname} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "H5IMPORT-${testname}")

    # If using memchecker add tests without using scripts
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5IMPORT-H5DMP-${testname}
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
      SET_TESTS_PROPERTIES (H5IMPORT-H5DMP-${testname} PROPERTIES DEPENDS H5IMPORT-${testname})
      ADD_TEST (
          NAME H5IMPORT-H5DMP_CMP-${testname}
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
      SET_TESTS_PROPERTIES (H5IMPORT-H5DMP_CMP-${testname} PROPERTIES DEPENDS H5IMPORT-H5DMP-${testname})
      SET (last_test "H5IMPORT-H5DMP_CMP-${testname}")
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_TEST testname resultcode testfile vparam srcname dstname)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

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
          txtin16.txt
          txtin32.txt
          ${HDF5_REFERENCE_TEST_FILES}
  )
  SET (last_test "H5IMPORT-clear-objects")

  ADD_TEST (NAME H5IMPORT-h5importtest COMMAND $<TARGET_FILE:h5importtest>)
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (H5IMPORT-h5importtest PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "H5IMPORT-h5importtest")

  # ----- TESTING "ASCII I32 rank 3 - Output BE " ;
  ADD_H5_TEST (ASCII_I32 txtin16.txt txtin32.conf txtin32.h5)

  # ----- TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
  ADD_H5_TEST (ASCII_I16 txtin16.txt txtin16.conf txtin16.h5)

  # ----- TESTING "ASCII I8 - rank 3 - Output I8 LE-Chunked+Extended+Compressed " 
  ADD_H5_TEST (ASCII_I8 txtin16.txt txtin8.conf txtin8.h5)

  # ----- TESTING "ASCII UI32 - rank 3 - Output BE" 
  ADD_H5_TEST (ASCII_UI32 txtuin32.txt txtuin32.conf txtuin32.h5)

  # ----- TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
  ADD_H5_TEST (ASCII_UI16 txtuin32.txt txtuin16.conf txtuin16.h5)

  # ----- TESTING "ASCII F32 - rank 3 - Output LE " 
  ADD_H5_TEST (ASCII_F32 txtfp32.txt txtfp32.conf txtfp32.h5)

  # ----- TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
  ADD_H5_TEST (ASCII_F64 txtfp64.txt txtfp64.conf txtfp64.h5)

  # ----- TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
  ADD_H5_TEST (BINARY_F64 binfp64.bin binfp64.conf binfp64.h5)

  # ----- TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
  ADD_H5_TEST (BINARY_I16 binin16.bin binin16.conf binin16.h5)

  # ----- TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
  ADD_H5_TEST (BINARY_I8 binin8.bin binin8.conf binin8.h5)

  # ----- TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
  ADD_H5_TEST (BINARY_I32 binin32.bin binin32.conf binin32.h5)

  # ----- TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
  ADD_H5_TEST (BINARY_UI16 binuin16.bin binuin16.conf binuin16.h5)

  # ----- TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
  ADD_H5_TEST (BINARY_UI32 binuin32.bin binuin32.conf binuin32.h5)

  # ----- TESTING "STR" 
  ADD_H5_TEST (STR txtstr.txt txtstr.conf txtstr.h5)

  # ----- TESTING "BINARY I8 CR LF EOF" 
  ADD_H5_TEST (BINARY_I8_EOF binin8w.bin binin8w.conf binin8w.h5)

  # ----- TESTING "ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE " 
  ADD_H5_TEST (ASCII_F64_R1 textpfe64.txt textpfe.conf textpfe.h5)

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
        h5import
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
