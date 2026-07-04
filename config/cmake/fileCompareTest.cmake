# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# -----------------------------------------------------------------------------
# fileCompareTest.cmake
#
# This script compares two files for HDF5 testing. It supports both string and
# size-based comparisons, and can be used to check if files are equal, less than,
# greater than, etc., depending on the TEST_FUNCTION argument.
#
# Required variables:
#   - TEST_FOLDER: Directory containing the files
#   - TEST_ONEFILE: Name of the first file
#   - TEST_TWOFILE: Name of the second file
#   - TEST_FUNCTION: Comparison function (LT, LTEQ, EQ, GTEQ, GT)
#   - TEST_STRINGS: If set to "YES", compare file contents as strings
#   - TEST_EXPECT: Expected result code for string comparison
#   - TEST_ERROR: Error message to display on failure
# -----------------------------------------------------------------------------

# Check that all required arguments are defined
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_ONEFILE)
  message (FATAL_ERROR "Require TEST_ONEFILE the first file to be defined")
endif ()
if (NOT TEST_TWOFILE)
  message (FATAL_ERROR "Require TEST_TWOFILE the second file to be defined")
endif ()
if (NOT TEST_FUNCTION)
  message (FATAL_ERROR "Require TEST_FUNCTION (LT,LTEQ,EQ,GTEQ,GT) to be defined")
endif ()

# Initialize variables for file sizes and string lengths
set (TEST_ONE_SIZE 0)
set (TEST_TWO_SIZE 0)
set (TEST_ONE_STRING 0)
set (TEST_TWO_STRING 0)
set (TEST_ONE_STRING_LEN 0)
set (TEST_TWO_STRING_LEN 0)

if (TEST_STRINGS STREQUAL "YES")
  # String-based comparison: read file contents and compare as strings
  file (STRINGS ${TEST_FOLDER}/${TEST_ONEFILE} TEST_ONE_STRING)
  string (LENGTH ${TEST_ONE_STRING} TEST_ONE_STRING_LEN)

  file (STRINGS ${TEST_FOLDER}/${TEST_TWOFILE} TEST_TWO_STRING)
  string (LENGTH ${TEST_TWO_STRING} TEST_TWO_STRING_LEN)

  math (EXPR TEST_STRING_SIZE "${TEST_ONE_STRING_LEN} - ${TEST_TWO_STRING_LEN}" )

  # Compare files, ignoring end-of-line differences
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${TEST_FOLDER}/${TEST_ONEFILE} ${TEST_FOLDER}/${TEST_TWOFILE}
      RESULT_VARIABLE TEST_RESULT
  )

  message (VERBOSE "COMPARE Result: ${TEST_RESULT}: ${TEST_STRING_SIZE}=${TEST_U_STRING_LEN}-${TEST_O_STRING_LEN}")
  # If the result does not match the expected value, fail
  if (NOT TEST_RESULT EQUAL TEST_EXPECT)
    message (FATAL_ERROR "Failed: The output of ${TEST_FOLDER}/${TEST_ONEFILE} did not match ${TEST_FOLDER}/${TEST_TWOFILE}.\n${TEST_ERROR}")
  endif ()
else ()
  # Size-based comparison: compare file sizes using the requested function
  file (SIZE ${TEST_FOLDER}/${TEST_ONEFILE} TEST_ONE_SIZE)
  file (SIZE ${TEST_FOLDER}/${TEST_TWOFILE} TEST_TWO_SIZE)
  if (TEST_FUNCTION MATCHES "LT")
    if (TEST_ONE_SIZE LESS TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was less ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT less ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "LTEQ")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was less or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT less or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "EQ")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "GTEQ")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was greater or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT greater or equal ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  elseif (TEST_FUNCTION MATCHES "GT")
    if (TEST_ONE_SIZE LESS_EQUAL TEST_TWO_SIZE)
      message (VERBOSE "Passed: The size of ${TEST_FOLDER}/${TEST_ONEFILE} was greater ${TEST_FOLDER}/${TEST_TWOFILE}")
    else ()
      message (FATAL_ERROR "The size of ${TEST_FOLDER}/${TEST_ONEFILE} was NOT greater ${TEST_FOLDER}/${TEST_TWOFILE}")
    endif ()
  else ()
    message (FATAL_ERROR "Failed: Incorrect test size compare command provided.\n${TEST_ERROR}")
  endif ()
endif ()

# everything went fine...
