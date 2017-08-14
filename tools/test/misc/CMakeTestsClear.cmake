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
      h5clear_log_v3.h5
      h5clear_mdc_image.h5
      mod_h5clear_mdc_image.h5
      latest_h5clear_log_v3.h5
      latest_h5clear_sec2_v3.h5
  )
  set (HDF5_SEC2_TEST_FILES
      h5clear_sec2_v0.h5
      h5clear_sec2_v2.h5
      h5clear_sec2_v3.h5
  )
  set (HDF5_REFERENCE_TEST_FILES
      h5clear_usage.ddl
      h5clear_open_fail.ddl
      h5clear_missing_file.ddl
      h5clear_no_mdc_image.ddl
  )

  foreach (h5_file ${HDF5_TEST_FILES} ${HDF5_SEC2_TEST_FILES} ${HDF5_REFERENCE_TEST_FILES})
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

  macro (ADD_H5_CMP testname resultfile resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_CMP-${testname}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
                  ${testname}.out
                  ${testname}.out.err
      )
      add_test (
          NAME H5CLEAR_CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5clear>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5CLEAR_CMP-${testname} PROPERTIES DEPENDS H5CLEAR_CMP-${testname}-clear-objects)
    endif ()
  endmacro ()

  macro (ADD_H5_RETTEST testname resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR_RET-${testname}
          COMMAND $<TARGET_FILE:h5clear> ${ARGN}
      )
      set_tests_properties (H5CLEAR_RET-${testname} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      set_tests_properties (H5CLEAR_RET-${testname} PROPERTIES WILL_FAIL "${resultcode}")
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (H5CLEAR_RET-${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "H5CLEAR_RET-${testname}")
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5CLEAR-clear_open_chk-copy_${testname}.h5
          COMMAND    ${CMAKE_COMMAND}
              -E copy_if_different
              "${PROJECT_SOURCE_DIR}/testfiles/${testname}.h5" "${PROJECT_BINARY_DIR}/testfiles/${testname}.h5"
      )
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (H5CLEAR-clear_open_chk-copy_${testname}.h5 PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "H5CLEAR-clear_open_chk-copy_${testname}.h5")
      # Initial file open fails OR
      # File open succeeds because the library does not check status_flags for file with < v3 superblock
      add_test (NAME H5CLEAR-clear_open_chk-${testname}_${resultcode} COMMAND $<TARGET_FILE:clear_open_chk> ${testname}.h5)
      set_tests_properties (H5CLEAR-clear_open_chk-${testname}_${resultcode} PROPERTIES WILL_FAIL "${resultcode}")
      set_tests_properties (H5CLEAR-clear_open_chk-${testname}_${resultcode} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (H5CLEAR-clear_open_chk-${testname}_${resultcode} PROPERTIES DEPENDS ${last_test})
      endif ()
      # After "h5clear" the file, the subsequent file open succeeds
      add_test (NAME H5CLEAR-h5clear-${testname} COMMAND $<TARGET_FILE:h5clear> -s ${testname}.h5)
      set_tests_properties (H5CLEAR-h5clear-${testname} PROPERTIES DEPENDS H5CLEAR-clear_open_chk-${testname}_${resultcode})
      set_tests_properties (H5CLEAR-h5clear-${testname} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      add_test (NAME H5CLEAR-clear_open_chk-${testname} COMMAND $<TARGET_FILE:clear_open_chk> ${testname}.h5)
      set_tests_properties (H5CLEAR-clear_open_chk-${testname} PROPERTIES DEPENDS H5CLEAR-h5clear-${testname})
      set_tests_properties (H5CLEAR-clear_open_chk-${testname} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
      set (last_test "H5CLEAR-clear_open_chk-${testname}")
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
# The following are tests to verify the status_flags field is cleared properly:
if (HDF5_ENABLE_USING_MEMCHECKER)
  # Remove any output file left over from previous test run
  add_test (
    NAME H5CLEAR-clearall-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        h5clear_log_v3.h5
        h5clear_mdc_image.h5
        h5clear_sec2_v0.h5
        h5clear_sec2_v2.h5
        h5clear_sec2_v3.h5
        orig_h5clear_sec2_v0.h5
        orig_h5clear_sec2_v2.h5
        orig_h5clear_sec2_v3.h5
        latest_h5clear_log_v3.h5
        latest_h5clear_sec2_v3.h5
        mod_h5clear_mdc_image.h5
        mod_h5clear_mdc_image2.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (H5CLEAR-clearall-objects PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "H5CLEAR-clearall-objects")

  foreach (h5_file ${HDF5_TEST_FILES} ${HDF5_SEC2_TEST_FILES})
    add_test (
      NAME H5CLEAR-copy_${h5_file}
      COMMAND    ${CMAKE_COMMAND}
          -E copy_if_different
          "${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}"
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5CLEAR-copy_${h5_file} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5CLEAR-copy_${h5_file}")
  endforeach ()
  # make second copy of h5clear_sec2.h5
  foreach (h5_file ${HDF5_SEC2_TEST_FILES})
    add_test (
      NAME H5CLEAR-copy_orig_${h5_file}
      COMMAND    ${CMAKE_COMMAND}
          -E copy_if_different
          "${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/orig_${h5_file}"
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (H5CLEAR-copy_orig_${h5_file} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5CLEAR-copy_orig_${h5_file}")
  endforeach ()
  # make second copy of mod_h5clear_mdc_image.h5
  add_test (
      NAME H5CLEAR-copy_mod_h5clear_mdc_image2.h5
      COMMAND    ${CMAKE_COMMAND}
          -E copy_if_different
          "${PROJECT_SOURCE_DIR}/testfiles/mod_h5clear_mdc_image.h5" "${PROJECT_BINARY_DIR}/testfiles/mod_h5clear_mdc_image2.h5"
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (H5CLEAR-copy_mod_h5clear_mdc_image2.h5 PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "H5CLEAR-copy_mod_h5clear_mdc_image2.h5")
endif()

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
  ADD_H5_CMP (h5clear_usage_h h5clear_usage 0 "-h")
  ADD_H5_CMP (h5clear_usage h5clear_usage 1 "")
  ADD_H5_CMP (h5clear_usage_junk h5clear_usage 1 "" junk.h5)
  ADD_H5_CMP (h5clear_usage_none h5clear_usage 1 "" orig_h5clear_sec2_v3.h5)
  ADD_H5_CMP (h5clear_missing_file_m h5clear_missing_file 1 "-m")
  ADD_H5_CMP (h5clear_open_fail_s h5clear_open_fail 1 "-s" junk.h5)
  ADD_H5_CMP (h5clear_missing_file_ms h5clear_missing_file 1 "-m" "-s")
  ADD_H5_CMP (h5clear_open_fail_ms h5clear_open_fail 1 "-m" "-s"  junk.h5)
  ADD_H5_CMP (h5clear_no_mdc_image_m h5clear_no_mdc_image 0 "-m" orig_h5clear_sec2_v2.h5)
  ADD_H5_CMP (h5clear_no_mdc_image_ms h5clear_no_mdc_image 0 "-s" "-m" orig_h5clear_sec2_v0.h5)
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
  ADD_H5_RETTEST (h5clear_mdc_image "false" "-m" h5clear_mdc_image.h5)
  ADD_H5_RETTEST (h5clear_vers "false" "--vers")
  ADD_H5_RETTEST (h5clear_k "true" "-k")
  ADD_H5_RETTEST (h5clear_k_junk "true" "-k" junk.h5)
  ADD_H5_RETTEST (h5clear_l_sec2 "true" "-l" h5clear_sec2_v2.h5)
  ADD_H5_RETTEST (h5clear_mk "true" "-m" "-k")
  ADD_H5_RETTEST (h5clear_lm "true" "-l" "-m")
  ADD_H5_RETTEST (h5clear_ml_junk "true" "-m" "-l" junk.h5)
  ADD_H5_RETTEST (h5clear_lm_junk "true" "-l" "-m" junk.h5)
  ADD_H5_RETTEST (h5clear_ml_sec2 "true" "-m" "-l" h5clear_sec2_v0.h5)
  ADD_H5_RETTEST (h5clear_lm_sec2 "true" "-l" "-m" h5clear_sec2_v0.h5)
#
#
#
# h5clear_mdc_image.h5 already has cache image removed earlier, verify the expected warning from h5clear:
  ADD_H5_CMP (h5clear_mdc_image_m h5clear_no_mdc_image 0 "-m" mod_h5clear_mdc_image.h5)
  ADD_H5_CMP (h5clear_mdc_image_sm h5clear_no_mdc_image 0 "-s" "-m" mod_h5clear_mdc_image2.h5)
#
#
#
# The following are tests to verify the status_flags field is cleared properly:
  ADD_H5_TEST (h5clear_sec2_v3 "true")
  ADD_H5_TEST (h5clear_log_v3 "true")
  ADD_H5_TEST (latest_h5clear_sec2_v3 "true")
  ADD_H5_TEST (latest_h5clear_log_v3 "true")
  ADD_H5_TEST (h5clear_sec2_v0 "false")
  ADD_H5_TEST (h5clear_sec2_v2 "false")
