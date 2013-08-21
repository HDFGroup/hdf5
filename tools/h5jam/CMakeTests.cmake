
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  SET (HDF5_REFERENCE_TXT_FILES
      u10.txt
      u511.txt
      u512.txt
      u513.txt
      h5jam-help.txt
      h5unjam-help.txt
      h5jam-ub-nohdf5.txt
  )
  SET (HDF5_REFERENCE_TEST_FILES
      tall.h5
      twithub.h5
      twithub513.h5
  )

  FILE (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5jam
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

  FOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/testfiles/${txt_file}")
    #MESSAGE (STATUS " Copying ${txt_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5jam
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/testfiles/${txt_file} ${dest}
    )
  ENDFOREACH (txt_file ${HDF5_REFERENCE_TXT_FILES})
    
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  # ============================================================
  # TEST_H5JAM_OUTPUT
  # For the purpose to verify only output & exitcode from h5jam
  #
  MACRO (TEST_H5JAM_OUTPUT expectfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5JAM-${expectfile} COMMAND $<TARGET_FILE:h5jam> ${ARGN})
      IF (NOT "${resultcode}" STREQUAL "0")
        SET_TESTS_PROPERTIES (H5JAM-${expectfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT "${resultcode}" STREQUAL "0")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-${expectfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${expectfile}.out
              ${expectfile}.out.err
      )
      ADD_TEST (
          NAME H5JAM-${expectfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5jam>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${expectfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=testfiles/${expectfile}.txt"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-${expectfile} PROPERTIES DEPENDS H5JAM-${expectfile}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (TEST_H5JAM_OUTPUT)

  # ============================================================
  # TEST_H5UNJAM_OUTPUT
  # For the purpose to verify only output & exitcode from h5unjam
  #
  MACRO (TEST_H5UNJAM_OUTPUT expectfile resultcode)
    # If using memchecker add tests without using scripts
    IF (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (NAME H5JAM-UNJAM-${expectfile} COMMAND $<TARGET_FILE:h5unjam> ${ARGN})
      IF (NOT "${resultcode}" STREQUAL "0")
        SET_TESTS_PROPERTIES (H5JAM-UNJAM-${expectfile} PROPERTIES WILL_FAIL "true")
      ENDIF (NOT "${resultcode}" STREQUAL "0")
    ELSE (HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-UNJAM-${expectfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${expectfile}.out
              ${expectfile}.out.err
      )
      ADD_TEST (
          NAME H5JAM-UNJAM-${expectfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5unjam>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${expectfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=testfiles/${expectfile}.txt"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-UNJAM-${expectfile} PROPERTIES DEPENDS H5JAM-UNJAM-${expectfile}-clear-objects)
    ENDIF (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (TEST_H5UNJAM_OUTPUT)

  MACRO (CHECKFILE testname testdepends expected actual)
    # If using memchecker add tests without using scripts
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-${testname}-CHECKFILE-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${actual}.new
              ${actual}.new.err
              ${actual}.out
              ${actual}.out.err
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-CHECKFILE-clear-objects PROPERTIES DEPENDS ${testdepends})
      ADD_TEST (
          NAME H5JAM-${testname}-CHECKFILE-H5DMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=testfiles/${expected}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${actual}.new"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_SKIP_COMPARE=TRUE"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-CHECKFILE-H5DMP PROPERTIES DEPENDS H5JAM-${testname}-CHECKFILE-clear-objects)
      ADD_TEST (
          NAME H5JAM-${testname}-CHECKFILE-H5DMP_CMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${actual}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=${actual}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_FILTER=(^(HDF5)[^\n]*)"
              -D "TEST_REFERENCE=${actual}.new"
              -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-CHECKFILE-H5DMP_CMP PROPERTIES DEPENDS H5JAM-${testname}-CHECKFILE-H5DMP)
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO(CHECKFILE testname testdepends expected actual)

  MACRO (UNJAMTEST testname setfile infile ufile chkfile outfile)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-${testname}-UNJAM-SETUP-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${infile}
      )
      ADD_TEST (
          NAME H5JAM-${testname}-UNJAM-SETUP
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/testfiles/${setfile} ${PROJECT_BINARY_DIR}/${infile}
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM-SETUP PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-SETUP-clear-objects)
      ADD_TEST (
          NAME H5JAM-${testname}-UNJAM-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${outfile}
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM-clear-objects PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-SETUP)
      IF (NOT "${ufile}" STREQUAL "NONE")
        ADD_TEST (
            NAME H5JAM-${testname}-UNJAM_D-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove ${ufile}
        )
        SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM_D-clear-objects PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-clear-objects)
        ADD_TEST (NAME H5JAM-${testname}-UNJAM COMMAND $<TARGET_FILE:h5unjam> -i ${infile} -u ${ufile} -o ${outfile})
        SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM PROPERTIES DEPENDS H5JAM-${testname}-UNJAM_D-clear-objects)
        SET (compare_test ${ufile})
      ELSE (NOT "${ufile}" STREQUAL "NONE")
        IF (NOT "${ARGN}" STREQUAL "--delete")
          ADD_TEST (
              NAME H5JAM-${testname}-UNJAM_D-clear-objects
              COMMAND ${CMAKE_COMMAND} -E remove ${outfile}.ufile.txt ${outfile}.ufile.txt.err
          )
          SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM_D-clear-objects PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-clear-objects)
          ADD_TEST (
              NAME H5JAM-${testname}-UNJAM
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5unjam>"
                  -D "TEST_ARGS:STRING=-i;${infile};-o;${outfile}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                  -D "TEST_OUTPUT=${outfile}.ufile.txt"
                  -D "TEST_EXPECT=0"
                  -D "TEST_SKIP_COMPARE=TRUE"
                  -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
          )
          SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM PROPERTIES DEPENDS H5JAM-${testname}-UNJAM_D-clear-objects)
          SET (compare_test "${outfile}.ufile.txt")
        ELSE (NOT "${ARGN}" STREQUAL "--delete")
          ADD_TEST (NAME H5JAM-${testname}-UNJAM COMMAND $<TARGET_FILE:h5unjam> -i ${infile} -o ${outfile})
          SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-clear-objects)
          SET (compare_test "")
        ENDIF (NOT "${ARGN}" STREQUAL "--delete")
      ENDIF (NOT "${ufile}" STREQUAL "NONE")
      IF (NOT "${compare_test}" STREQUAL "")
        ADD_TEST (
            NAME H5JAM-${testname}-UNJAM-CHECK_UB_1-clear-objects
            COMMAND    ${CMAKE_COMMAND}
                -E remove
                ${infile}.len.txt
                ${infile}.cmp
                ${infile}-ub.cmp
        )
        SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM-CHECK_UB_1-clear-objects PROPERTIES DEPENDS "H5JAM-${testname}-UNJAM")
        ADD_TEST (
            NAME H5JAM-${testname}-UNJAM-CHECK_UB_1
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_PROGRAM=$<TARGET_FILE:tellub>"
                -D "TEST_GET_PROGRAM=$<TARGET_FILE:getub>"
                -D "TEST_CHECKUB=YES"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_HFILE=${infile}"
                -D "TEST_UFILE=${compare_test}"
                -D "TEST_EXPECT=0"
                -D "TEST_OFILE="
                -P "${HDF5_RESOURCES_DIR}/userblockTest.cmake"
        )
        SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM-CHECK_UB_1 PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-CHECK_UB_1-clear-objects)
      ENDIF (NOT "${compare_test}" STREQUAL "")

      ADD_TEST (
          NAME H5JAM-${testname}-UNJAM-CHECK_NOUB
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:tellub>"
              -D "TEST_GET_PROGRAM=$<TARGET_FILE:getub>"
              -D "TEST_CHECKUB=NO"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_HFILE=${outfile}"
              -D "TEST_EXPECT=0"
              -D "TEST_UFILE=NULL"
              -D "TEST_OFILE=NULL"
              -P "${HDF5_RESOURCES_DIR}/userblockTest.cmake"
      )
      IF (NOT "${compare_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM-CHECK_NOUB PROPERTIES DEPENDS H5JAM-${testname}-UNJAM-CHECK_UB_1)
      ELSE (NOT "${compare_test}" STREQUAL "")
        SET_TESTS_PROPERTIES (H5JAM-${testname}-UNJAM-CHECK_NOUB PROPERTIES DEPENDS H5JAM-${testname}-UNJAM)
      ENDIF (NOT "${compare_test}" STREQUAL "")

      CHECKFILE (${testname} "H5JAM-${testname}-UNJAM-CHECK_NOUB" ${chkfile} ${outfile})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO(UNJAMTEST testname infile ufile outfile)

  MACRO (JAMTEST testname jamfile infile chkfile outfile)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ${outfile} ${infile}.cpy.h5
      )
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
    ADD_TEST (NAME H5JAM-${testname} COMMAND $<TARGET_FILE:h5jam> -u testfiles/${jamfile} -i testfiles/${infile} -o ${outfile} ${ARGN})
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      SET_TESTS_PROPERTIES (H5JAM-${testname} PROPERTIES DEPENDS H5JAM-${testname}-clear-objects)
      SET (compare_test ${outfile})
      SET (compare_orig testfiles/${infile})
      IF ("${ARGN}" STREQUAL "--clobber")
        SET (compare_orig "")
      ENDIF ("${ARGN}" STREQUAL "--clobber")

      ADD_TEST (
          NAME H5JAM-${testname}-CHECK_UB_1-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${compare_test}.len.txt
              ${compare_test}.cmp
              ${compare_test}-ub.cmp
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-CHECK_UB_1-clear-objects PROPERTIES DEPENDS "H5JAM-${testname}")
      ADD_TEST (
          NAME H5JAM-${testname}-CHECK_UB_1
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:tellub>"
              -D "TEST_GET_PROGRAM=$<TARGET_FILE:getub>"
              -D "TEST_CHECKUB=YES"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_HFILE=${compare_test}"
              -D "TEST_UFILE=testfiles/${jamfile}"
              -D "TEST_EXPECT=0"
              -D "TEST_OFILE=${compare_orig}"
              -P "${HDF5_RESOURCES_DIR}/userblockTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}-CHECK_UB_1 PROPERTIES DEPENDS H5JAM-${testname}-CHECK_UB_1-clear-objects)
      CHECKFILE (${testname} "H5JAM-${testname}-CHECK_UB_1" ${chkfile} ${outfile})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (JAMTEST testname jamfile infile outfile)

  MACRO (JAMTEST_NONE testname jamfile infile setfile chkfile)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      ADD_TEST (
          NAME H5JAM-${testname}_NONE-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove 
              ${chkfile} ${chkfile}.cpy.h5
      )
      ADD_TEST (
          NAME H5JAM-${testname}_NONE-SETUP
          COMMAND ${CMAKE_COMMAND} -E copy_if_different testfiles/${setfile} ${chkfile}
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}_NONE-SETUP PROPERTIES DEPENDS H5JAM-${testname}_NONE-clear-objects)

      ADD_TEST (
          NAME H5JAM-${testname}_NONE_COPY
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${chkfile} ${chkfile}.cpy.h5
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}_NONE_COPY PROPERTIES DEPENDS H5JAM-${testname}_NONE-SETUP)

      ADD_TEST (NAME H5JAM-${testname}_NONE COMMAND $<TARGET_FILE:h5jam> -u testfiles/${jamfile} -i ${chkfile} ${ARGN})
      SET_TESTS_PROPERTIES (H5JAM-${testname}_NONE PROPERTIES DEPENDS H5JAM-${testname}_NONE_COPY)

      SET (compare_test ${chkfile})
      SET (compare_orig ${chkfile}.cpy.h5)
      IF ("${ARGN}" STREQUAL "--clobber")
        SET (compare_orig "")
      ENDIF ("${ARGN}" STREQUAL "--clobber")

      ADD_TEST (
          NAME H5JAM-${testname}_NONE-CHECK_UB_1-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
              ${compare_test}.len.txt
              ${compare_test}.cmp
              ${compare_test}-ub.cmp
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}_NONE-CHECK_UB_1-clear-objects PROPERTIES DEPENDS "H5JAM-${testname}_NONE")
      ADD_TEST (
          NAME H5JAM-${testname}_NONE-CHECK_UB_1
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:tellub>"
              -D "TEST_GET_PROGRAM=$<TARGET_FILE:getub>"
              -D "TEST_CHECKUB=YES"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_HFILE=${compare_test}"
              -D "TEST_UFILE=testfiles/${jamfile}"
              -D "TEST_EXPECT=0"
              -D "TEST_OFILE=${compare_orig}"
              -P "${HDF5_RESOURCES_DIR}/userblockTest.cmake"
      )
      SET_TESTS_PROPERTIES (H5JAM-${testname}_NONE-CHECK_UB_1 PROPERTIES DEPENDS H5JAM-${testname}_NONE-CHECK_UB_1-clear-objects)
      CHECKFILE (${testname} "H5JAM-${testname}_NONE-CHECK_UB_1" ${infile} ${chkfile})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (JAMTEST_NONE testname jamfile infile setfile chkfile)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

#-------------------------------
# Testing h5jam
#-------------------------------
  # help page
  TEST_H5JAM_OUTPUT(h5jam-help 0 -h)

  # don't allow HDF5 format file as an user block file
  TEST_H5JAM_OUTPUT(h5jam-ub-nohdf5 1 -i testfiles/tall.h5 -u testfiles/tall.h5 -o tall-tmp.h5)

  JAMTEST (tall_u10 u10.txt tall.h5 tall.h5 ta2.h5)
  JAMTEST (tall_u511 u511.txt tall.h5 tall.h5 ta3.h5)
  JAMTEST (tall_u512 u512.txt tall.h5 tall.h5 ta4.h5)
  JAMTEST (tall_u513 u513.txt tall.h5 tall.h5 ta5.h5)

  JAMTEST_NONE (N_ta_u10 u10.txt tall.h5 tall.h5 ta6.h5)
  JAMTEST_NONE (N_ta_u511 u511.txt tall.h5 tall.h5 ta7.h5)
  JAMTEST_NONE (N_ta_u512 u512.txt tall.h5 tall.h5 ta8.h5)
  JAMTEST_NONE (N_ta_u513 u513.txt tall.h5 tall.h5 ta9.h5)

  JAMTEST (twithub_u10 u10.txt twithub.h5 tall.h5 tax2.h5)
  JAMTEST (twithub_u511 u511.txt twithub.h5 tall.h5 tax3.h5)
  JAMTEST (twithub_u512 u512.txt twithub.h5 tall.h5 tax4.h5)
  JAMTEST (twithub_u513 u513.txt twithub.h5 tall.h5 tax5.h5)

  JAMTEST (twithub513_u10 u10.txt twithub513.h5 tall.h5 tax6.h5)
  JAMTEST (twithub513_u511 u511.txt twithub513.h5 tall.h5 tax7.h5)
  JAMTEST (twithub513_u512 u512.txt twithub513.h5 tall.h5 tax8.h5)
  JAMTEST (twithub513_u513 u513.txt twithub513.h5 tall.h5 tax9.h5)

  JAMTEST (twithub_u10_c u10.txt twithub.h5 tall.h5 taz2.h5 --clobber)
  JAMTEST (twithub_u511_c u511.txt twithub.h5 tall.h5 taz3.h5 --clobber)
  JAMTEST (twithub_u512_c u512.txt twithub.h5 tall.h5 taz4.h5 --clobber)
  JAMTEST (twithub_u513_c u513.txt twithub.h5 tall.h5 taz5.h5 --clobber)

  JAMTEST (twithub513_u10_c u10.txt twithub513.h5 tall.h5 taz6.h5 --clobber)
  JAMTEST (twithub513_u511_c u511.txt twithub513.h5 tall.h5 taz7.h5 --clobber)
  JAMTEST (twithub513_u512_c u512.txt twithub513.h5 tall.h5 taz8.h5 --clobber)
  JAMTEST (twithub513_u513_c u513.txt twithub513.h5 tall.h5 taz9.h5 --clobber)

  JAMTEST_NONE (N_twithub_u10_c u10.txt tall.h5 twithub.h5 tay2.h5 --clobber)
  JAMTEST_NONE (N_twithub_u511_c u511.txt tall.h5 twithub.h5 tay3.h5 --clobber)
  JAMTEST_NONE (N_twithub_u512_c u512.txt tall.h5 twithub.h5 tay4.h5 --clobber)
  JAMTEST_NONE (N_twithub_u513_c u513.txt tall.h5 twithub.h5 tay5.h5 --clobber)

  JAMTEST_NONE (N_twithub513_u10_c u10.txt tall.h5 twithub513.h5 tay6.h5 --clobber)
  JAMTEST_NONE (N_twithub513_u511_c u511.txt tall.h5 twithub513.h5 tay7.h5 --clobber)
  JAMTEST_NONE (N_twithub513_u512_c u512.txt tall.h5 twithub513.h5 tay8.h5 --clobber)
  JAMTEST_NONE (N_twithub513_u513_c u513.txt tall.h5 twithub513.h5 tay9.h5 --clobber)

#-------------------------------
# Testing h5unjam
#-------------------------------
  # help page
  TEST_H5UNJAM_OUTPUT(h5unjam-help 0 -h)

  UNJAMTEST (twithub_tall twithub.h5 tai1.h5 o10.txt tall.h5 taa1.h5)
  UNJAMTEST (twithub513_tall twithub513.h5 tai2.h5 o512.txt tall.h5 taa2.h5)

  UNJAMTEST (N_twithub_tall twithub.h5 tai3.h5 NONE tall.h5 taa3.h5)
  UNJAMTEST (N_twithub513_tall twithub513.h5 tai4.h5 NONE tall.h5 taa4.h5)

  UNJAMTEST (D_twithub_tall twithub.h5 taj2.h5 NONE tall.h5 tac2.h5 --delete)
  UNJAMTEST (D_twithub513_tall twithub513.h5 taj3.h5 NONE tall.h5 tac3.h5 --delete)
