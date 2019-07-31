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
  set (HDF5_TEST_FILES
      h5clear_fsm_persist_equal.h5
      h5clear_fsm_persist_greater.h5
      h5clear_fsm_persist_less.h5
      h5clear_fsm_persist_noclose.h5
      h5clear_fsm_persist_user_equal.h5
      h5clear_fsm_persist_user_greater.h5
      h5clear_fsm_persist_user_less.h5
      h5clear_log_v3.h5
      h5clear_mdc_image.h5
      h5clear_status_noclose.h5
      latest_h5clear_log_v3.h5
      latest_h5clear_sec2_v3.h5
      mod_h5clear_mdc_image.h5
  )
  set (HDF5_SEC2_TEST_FILES
      h5clear_sec2_v0.h5
      h5clear_sec2_v2.h5
      h5clear_sec2_v3.h5
  )
  set (HDF5_REFERENCE_TEST_FILES
      h5clear_equal_after_size.ddl
      h5clear_equal_before_size.ddl
      h5clear_greater_after_size.ddl
      h5clear_greater_before_size.ddl
      h5clear_less_after_size.ddl
      h5clear_less_before_size.ddl
      h5clear_missing_file.ddl
      h5clear_noclose_after_size.ddl
      h5clear_noclose_before_size.ddl
      h5clear_status_noclose_after_size.ddl
      h5clear_usage.ddl
      h5clear_user_equal_after_size.ddl
      h5clear_user_equal_before_size.ddl
      h5clear_user_greater_after_size.ddl
      h5clear_user_greater_before_size.ddl
      h5clear_user_less_after_size.ddl
      h5clear_user_less_before_size.ddl
  )
  set (HDF5_REFERENCE_ERR_FILES
      h5clear_no_mdc_image.err
      h5clear_open_fail.err
  )

  foreach (h5_file ${HDF5_TEST_FILES} ${HDF5_SEC2_TEST_FILES} ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5clear_files")
  endforeach ()
  foreach (h5_file ${HDF5_REFERENCE_ERR_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5clear_files")
  endforeach ()
  # make second copy of h5clear_sec2.h5
  foreach (h5_file ${HDF5_SEC2_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/orig_${h5_file}" "h5clear_files")
  endforeach ()
  # make second copy of mod_h5clear_mdc_image.h5
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/mod_h5clear_mdc_image.h5" "${PROJECT_BINARY_DIR}/testfiles/mod_h5clear_mdc_image2.h5" "h5clear_files")
  add_custom_target(h5clear_files ALL COMMENT "Copying files needed by h5clear tests" DEPENDS ${h5clear_files_list})

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

  # Need special dependencies for tests that use the same reference file
  # This is an issue on Windows
  macro (ADD_H5_CMP testname resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_ERR_CMP testname resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.mty"
              -D "TEST_ERRREF=${resultfile}.err"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_CMP_WITH_COPY testname resultcode resultfile testfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_CMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testfile}
      )
      add_test (
          NAME H5CLEAR_CMP-copy_${testname}
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${testfile}" "${PROJECT_BINARY_DIR}/testfiles/${testfile}"
      )
      set_tests_properties (H5CLEAR_CMP-copy_${testname} PROPERTIES
          DEPENDS H5CLEAR_CMP-${testname}-clear-objects
      )
      add_test (
          NAME H5CLEAR_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN};${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_CMP-${testname} PROPERTIES
          DEPENDS H5CLEAR_CMP-copy_${testname}
      )
    endif ()
  endmacro ()

  macro (ADD_H5_ERR_CMP_WITH_COPY testname resultcode resultfile testfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_CMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testfile}
      )
      add_test (
          NAME H5CLEAR_CMP-copy_${testname}
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${testfile}" "${PROJECT_BINARY_DIR}/testfiles/${testfile}"
      )
      set_tests_properties (H5CLEAR_CMP-copy_${testname} PROPERTIES
          DEPENDS H5CLEAR_CMP-${testname}-clear-objects
      )
      add_test (
          NAME H5CLEAR_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=${ARGN};${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.mty"
              -D "TEST_ERRREF=${resultfile}.err"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_CMP-${testname} PROPERTIES
          DEPENDS H5CLEAR_CMP-copy_${testname}
      )
    endif ()
  endmacro ()

  macro (ADD_H5_RETTEST testname resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_RET-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear${tgt_ext}> ${ARGN}
      )
      set_tests_properties (H5CLEAR_RET-${testname} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          WILL_FAIL "${resultcode}"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_FILESIZE_TEST testname resultcode resultfile incr_size)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_FILESIZE_TEST-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}.h5
      )
      add_test (
          NAME H5CLEAR_FILESIZE_TEST-copy_${testname}
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${testname}.h5" "${PROJECT_BINARY_DIR}/testfiles/${testname}.h5"
      )
      set_tests_properties (H5CLEAR_FILESIZE_TEST-copy_${testname} PROPERTIES
          DEPENDS H5CLEAR_FILESIZE_TEST-${testname}-clear-objects
      )
      add_test (
          NAME H5CLEAR_FILESIZE_CMP-${testname}_before_size
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=--filesize;${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}_before_size.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}_before_size.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_FILESIZE_CMP-${testname}_before_size PROPERTIES
          DEPENDS H5CLEAR_FILESIZE_TEST-copy_${testname}
      )
      if (NOT ${incr_size} MATCHES "NONE")
          add_test (
              NAME H5CLEAR_FILESIZE_INCR-${testname}
              COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear${tgt_ext}> --increment=${incr_size} ${testname}.h5
          )
      else ()
          add_test (
              NAME H5CLEAR_FILESIZE_INCR-${testname}
              COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear${tgt_ext}> --increment ${testname}.h5
          )
      endif ()
      set_tests_properties (H5CLEAR_FILESIZE_INCR-${testname} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          WILL_FAIL "${resultcode}"
          DEPENDS H5CLEAR_FILESIZE_CMP-${testname}_before_size
      )
      add_test (
          NAME H5CLEAR_FILESIZE_CMP-${testname}_after_size
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=--filesize;${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}_after_size.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${resultfile}_after_size.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_FILESIZE_CMP-${testname}_after_size PROPERTIES
          DEPENDS H5CLEAR_FILESIZE_INCR-${testname}
      )
    endif ()
  endmacro ()

  macro (ADD_H5_FILESIZE_FAIL_TEST testname resultcode resultfile incr_size)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_FILESIZE_FAIL_TEST-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}.h5
      )
      add_test (
          NAME H5CLEAR_FILESIZE_FAIL_TEST-copy_${testname}
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${testname}.h5" "${PROJECT_BINARY_DIR}/testfiles/${testname}.h5"
      )
      set_tests_properties (H5CLEAR_FILESIZE_FAIL_TEST-copy_${testname} PROPERTIES
          DEPENDS H5CLEAR_FILESIZE_FAIL_TEST-${testname}-clear-objects
      )
      add_test (
          NAME H5CLEAR_FILESIZE_FAIL_CMP-${testname}_before_size
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=--filesize;${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}_before_size.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.mty"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_FILESIZE_FAIL_CMP-${testname}_before_size PROPERTIES
          DEPENDS H5CLEAR_FILESIZE_FAIL_TEST-copy_${testname}
      )
      if (NOT ${incr_size} MATCHES "NONE")
          add_test (
              NAME H5CLEAR_FILESIZE_FAIL_INCR-${testname}
              COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear${tgt_ext}> -s --increment=${incr_size} ${testname}.h5
          )
      else ()
          add_test (
              NAME H5CLEAR_FILESIZE_FAIL_INCR-${testname}
              COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear${tgt_ext}> -s --increment ${testname}.h5
          )
      endif ()
      set_tests_properties (H5CLEAR_FILESIZE_FAIL_INCR-${testname} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5CLEAR_FILESIZE_FAIL_CMP-${testname}_before_size
      )
      add_test (
          NAME H5CLEAR_FILESIZE_FAIL_CMP-${testname}_after_size
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear${tgt_ext}>"
              -D "TEST_ARGS:STRING=--filesize;${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}_after_size.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${resultfile}_after_size.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_FILESIZE_FAIL_CMP-${testname}_after_size PROPERTIES
          DEPENDS H5CLEAR_FILESIZE_FAIL_INCR-${testname}
      )
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname testfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR-clr_open_chk-copy_${testname}.h5
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${testfile}.h5" "${PROJECT_BINARY_DIR}/testfiles/${testfile}.h5"
      )

      # Initial file open fails OR
      # File open succeeds because the library does not check status_flags for file with < v3 superblock
      add_test (
          NAME H5CLEAR-clr_open_chk-${testname}_${resultcode}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:clear_open_chk> ${testfile}.h5
      )
      set_tests_properties (H5CLEAR-clr_open_chk-${testname}_${resultcode} PROPERTIES
          WILL_FAIL "${resultcode}"
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
          DEPENDS H5CLEAR-clr_open_chk-copy_${testname}.h5
      )

      # After "h5clear" the file, the subsequent file open succeeds
      add_test (
          NAME H5CLEAR-h5clr-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear${tgt_ext}> -s ${testfile}.h5
      )
      set_tests_properties (H5CLEAR-h5clr-${testname} PROPERTIES
          DEPENDS H5CLEAR-clr_open_chk-${testname}_${resultcode}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
      )
      add_test (
          NAME H5CLEAR-clr_open_chk-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:clear_open_chk> ${testfile}.h5
      )
      set_tests_properties (H5CLEAR-clr_open_chk-${testname} PROPERTIES
          DEPENDS H5CLEAR-h5clr-${testname}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################
#
#
#
# The following are tests to verify the expected output from h5clear
# "h5clear -h"
# "h5clear"                                 (no options, no file)
# "h5clear junk.h5"                         (no options, nonexisting file)
# "h5clear orig_h5clear_sec2_v3.h5"         (no options, existing file)
# "h5clear -m"                              (valid 1 option, no file)
# "h5clear -s junk.h5"                      (valid 1 option, nonexisting file)
# "h5clear -m -s"                           (valid 2 options, no file)
# "h5clear -m -s junk.h5"                   (valid 2 options, nonexisting file)
# "h5clear -m orig_h5clear_sec2_v2.h5"      (valid 1 option, existing file, no cache image)
# "h5clear -s -m orig_h5clear_sec2_v0.h5"   (valid 2 options, existing file, no cache image)
  ADD_H5_CMP (h5clr_usage_h h5clear_usage 0 "-h")
  ADD_H5_CMP (h5clr_usage h5clear_usage 1 "")
  ADD_H5_CMP (h5clr_usage_junk h5clear_usage 1 "" junk.h5)
  ADD_H5_CMP (h5clr_usage_none h5clear_usage 1 "" orig_h5clear_sec2_v3.h5)
  ADD_H5_CMP (h5clr_missing_file_m h5clear_missing_file 1 "-m")
  ADD_H5_ERR_CMP (h5clr_open_fail_s h5clear_open_fail 1 "-s" junk.h5)
  ADD_H5_CMP (h5clr_missing_file_ms h5clear_missing_file 1 "-m" "-s")
  ADD_H5_ERR_CMP (h5clr_open_fail_ms h5clear_open_fail 1 "-m" "-s"  junk.h5)
  ADD_H5_ERR_CMP (h5clr_no_mdc_image_m h5clear_no_mdc_image 0 "-m" orig_h5clear_sec2_v2.h5)
  ADD_H5_ERR_CMP (h5clr_no_mdc_image_ms h5clear_no_mdc_image 0 "-s" "-m" orig_h5clear_sec2_v0.h5)
#
#
#
# The following are tests to verify the expected exit code from h5clear:
# "h5clear -m h5clear_mdc_image.h5"     (valid option, existing file, succeed exit code)
# "h5clear --vers"                      (valid option, version #, succeed exit code)
# "h5clear -k"                          (invalid 1 option, no file, fail exit code)
# "h5clear -k junk.h5"                  (invalid 1 option, nonexisting file, fail exit code)
# "h5clear -l h5clear_sec2_v2.h5"       (invalid 1 option, existing file, fail exit code)
# "h5clear -m -k"                       (valid/invalid 2 options, nofile, fail exit code)
# "h5clear -l -m"                       (invalid/valid 2 options, nofile, fail exit code)
# "h5clear -m -l junk.h5"               (valid/invalid 2 options, nonexisting file, fail exit code)
# "h5clear -l -m junk.h5"               (invalid/valid 2 options, nonexisting file, fail exit code)
# "h5clear -m -l h5clear_sec2_v0.h5"    (valid/invalid 2 options, existing file, fail exit code)
# "h5clear -l -m h5clear_sec2_v0.h5"    (invalid/valid 2 options, existing file, fail exit code)
  ADD_H5_RETTEST (h5clr_mdc_image "false" "-m" h5clear_mdc_image.h5)
  ADD_H5_RETTEST (h5clr_vers "false" "--vers")
  ADD_H5_RETTEST (h5clr_k "true" "-k")
  ADD_H5_RETTEST (h5clr_k_junk "true" "-k" junk.h5)
  ADD_H5_RETTEST (h5clr_l_sec2 "true" "-l" h5clear_sec2_v2.h5)
  ADD_H5_RETTEST (h5clr_mk "true" "-m" "-k")
  ADD_H5_RETTEST (h5clr_lm "true" "-l" "-m")
  ADD_H5_RETTEST (h5clr_ml_junk "true" "-m" "-l" junk.h5)
  ADD_H5_RETTEST (h5clr_lm_junk "true" "-l" "-m" junk.h5)
  ADD_H5_RETTEST (h5clr_ml_sec2 "true" "-m" "-l" h5clear_sec2_v0.h5)
  ADD_H5_RETTEST (h5clr_lm_sec2 "true" "-l" "-m" h5clear_sec2_v0.h5)
#
#
#
# h5clear_mdc_image.h5 already has cache image removed earlier, verify the expected warning from h5clear:
  ADD_H5_ERR_CMP (h5clr_mdc_image_m h5clear_no_mdc_image 0 "-m" mod_h5clear_mdc_image.h5)
  ADD_H5_ERR_CMP (h5clr_mdc_image_sm h5clear_no_mdc_image 0 "-s" "-m" mod_h5clear_mdc_image2.h5)
#
#
#
# The following are tests to verify the status_flags field is cleared properly:
  ADD_H5_TEST (h5clr_sec2_v3 h5clear_sec2_v3 "true")
  ADD_H5_TEST (h5clr_log_v3 h5clear_log_v3 "true")
  ADD_H5_TEST (latest_h5clr_sec2_v3 latest_h5clear_sec2_v3 "true")
  ADD_H5_TEST (latest_h5clr_log_v3 latest_h5clear_log_v3 "true")
  ADD_H5_TEST (h5clr_sec2_v0 h5clear_sec2_v0 "false")
  ADD_H5_TEST (h5clr_sec2_v2 h5clear_sec2_v2 "false")
#
#
#
# The following tests verify the filesize, increment the filesize, then verify the filesize again.
#
# (1) h5clear_status_noclose.h5
# "h5clear --filesize h5clear_status_noclose.h5"        (unable to open the file because status_flags is enabled)
# "h5clear -s --increment=0 h5clear_status_noclose.h5"  (clear status_flag, EOA = MAX(EOA, EOF) + 0)
#                                                       (no output, check exit code)
# "h5clear --filesize h5clear_status_noclose.h5"        (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_FAIL_TEST (h5clear_status_noclose 1 h5clear_status_noclose 0)
#
# (2) h5clear_fsm_persist_noclose.h5
# "h5clear --filesize h5clear_fsm_persist_noclose.h5"       (print EOA/EOF before the next action)
# "h5clear --increment=0 h5clear_fsm_persist_noclose.h5"    (EOA = MAX(EOA, EOF)) (no output, just check exit code)
# "h5clear --filesize h5clear_fsm_persist_noclose.h5"       (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_noclose 0 h5clear_noclose 0)
#
# (3) h5clear_fsm_persist_equal.h5
# "h5clear --filesize h5clear_fsm_persist_equal.h5"     (print EOA/EOF before the next action)
# "h5clear --increment h5clear_fsm_persist_equal.h5"    (EOA = MAX(EOA, EOF) + 1M) (no output, check exit code)
# "h5clear --filesize h5clear_fsm_persist_equal.h5"     (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_equal 0 h5clear_equal NONE)
#
# (4) h5clear_fsm_persist_greater.h5
# "h5clear --filesize h5clear_fsm_persist_greater.h5"       (print EOA/EOF before the next action)
# "h5clear --increment=0 h5clear_fsm_persist_greater.h5"    (EOA = MAX(EOA, EOF) + 0) (no output, check exit code)
# "h5clear --filesize h5clear_fsm_persist_greater.h5"       (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_greater 0 h5clear_greater 0)
#
# (5) h5clear_fsm_persist_less.h5
# "h5clear --filesize h5clear_fsm_persist_less.h5"      (print EOA/EOF before the next action)
# "h5clear --increment=200 h5clear_fsm_persist_less.h5" (EOA = MAX(EOA, EOF) + 200) (no output, check exit code)
# "h5clear --filesize h5clear_fsm_persist_less.h5"      (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_less 0 h5clear_less 200)
#
# (6) h5clear_fsm_persist_user_equal.h5
# "h5clear --filesize h5clear_fsm_persist_user_equal.h5"    (print EOA/EOF before the next action)
# "h5clear --increment h5clear_fsm_persist_user_equal.h5"   (EOA = MAX(EOA, EOF) + 1M) (no output, check exit code)
# "h5clear --filesize h5clear_fsm_persist_user_equal.h5"    (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_user_equal 0 h5clear_user_equal NONE)
#
# (7) h5clear_fsm_persist_user_greater.h5
# "h5clear --filesize h5clear_fsm_persist_user_greater.h5"      (print EOA/EOF before the next action)
# "h5clear --increment=0 h5clear_fsm_persist_user_greater.h5"   (EOA = MAX(EOA, EOF) + 0) (no output, check exit code)
# "h5clear --filesize h5clear_fsm_persist_user_greater.h5"      (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_user_greater 0 h5clear_user_greater 0)
#
# (8) h5clear_fsm_persist_user_less.h5
# "h5clear --filesize h5clear_fsm_persist_user_less.h5"         (print EOA/EOF before the next action)
# "h5clear --increment=200 h5clear_fsm_persist_user_less.h5"    (EOA = MAX(EOA, EOF) + 200) (no output, check exit code)
# "h5clear --filesize h5clear_fsm_persist_user_less.h5"         (print EOA/EOF after the last action)
  ADD_H5_FILESIZE_TEST (h5clear_fsm_persist_user_less 0 h5clear_user_less 200)
#
#
