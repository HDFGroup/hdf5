#
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
# userblockTest.cmake - Test script for HDF5 user block functionality
#
# This script executes a command to extract or check user blocks in HDF5 files,
# captures the output, and compares it against reference files. It supports
# both checking and non-checking modes, and can validate user block content and size.
#
# Required variables:
#   - TEST_PROGRAM: Program to get user block size (e.g., tellub)
#   - TEST_GET_PROGRAM: Program to extract user block (e.g., getub)
#   - TEST_FOLDER: Directory containing test files
#   - TEST_HFILE: HDF5 file to check
#   - TEST_UFILE: User block file to check
#   - TEST_CHECKUB: YES/NO, whether to check user block content
# Optional variables:
#   - TEST_OFILE: (optional) Original HDF5 file for comparison
#   - TEST_EXPECT: (optional) Expected result code for comparison
#
# userblockTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.

# Check that all required arguments are defined
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM tellub to be defined")
endif ()
if (NOT TEST_GET_PROGRAM)
  message (FATAL_ERROR "Require TEST_GET_PROGRAM getub to be defined")
endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_HFILE)
  message (FATAL_ERROR "Require TEST_HFILE the hdf file to be defined")
endif ()
if (NOT TEST_UFILE)
  message (FATAL_ERROR "Require TEST_UFILE the ub file to be defined")
endif ()
if (NOT TEST_CHECKUB)
  message (STATUS "Require TEST_CHECKUB - YES or NO - to be defined")
endif ()
# TEST_EXPECT and TEST_OFILE are optional, depending on test mode

# Initialize variables for string lengths and sizes
set (TEST_U_STRING_LEN 0)
set (TEST_O_STRING_LEN 0)
set (TEST_H_STRING_LEN 0)
set (TEST_STRING_SIZE 0)

if (TEST_CHECKUB STREQUAL "YES")
  # --- User block content checking mode ---
  # Get the length of the user block file
  file (STRINGS ${TEST_FOLDER}/${TEST_UFILE} TEST_U_STRING)
  string (LENGTH ${TEST_U_STRING} TEST_U_STRING_LEN)

  # If an original file is provided, get its user block size
  if (TEST_OFILE)
    # Run tellub to get the user block size of the original file
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_OFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_HFILE}.len.txt
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )
    if (TEST_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} ${TEST_OFILE} is: ${TEST_ERROR}")
    endif ()
    # Read the user block size from the output file
    if (EXISTS "${TEST_HFILE}.len.txt")
      file (READ ${TEST_HFILE}.len.txt TEST_O_STRING_LEN)
    endif ()
  endif ()

  # Compute the total size to check (user block + original block)
  math( EXPR TEST_STRING_SIZE "${TEST_U_STRING_LEN} + ${TEST_O_STRING_LEN}" )

  if (TEST_O_STRING_LEN)
    # If there is an original user block, extract it and append the new user block
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_GET_PROGRAM} -c ${TEST_O_STRING_LEN} ${TEST_OFILE}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_HFILE}-ub.cmp
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    # Append the new user block to the comparison file
    file (STRINGS ${TEST_UFILE} TEST_STREAM NEWLINE_CONSUME)
    file (APPEND ${TEST_HFILE}-ub.cmp "${TEST_STREAM}")
  else ()
    # If no original user block, just write the new user block to the comparison file
    file (STRINGS ${TEST_UFILE} TEST_STREAM NEWLINE_CONSUME)
    file (WRITE ${TEST_HFILE}-ub.cmp ${TEST_STREAM})
  endif ()

  # Extract the combined user block from the test HDF5 file
  execute_process (
      COMMAND ${TEST_EMULATOR} ${TEST_GET_PROGRAM} -c ${TEST_STRING_SIZE} ${TEST_HFILE}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_RESULT
      OUTPUT_FILE ${TEST_HFILE}.cmp
      OUTPUT_VARIABLE TEST_OUT
      ERROR_VARIABLE TEST_ERROR
      OUTPUT_STRIP_TRAILING_WHITESPACE
  )

  # Compare the expected and actual user block outputs, ignoring EOL differences
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${TEST_HFILE}-ub.cmp ${TEST_HFILE}.cmp
      RESULT_VARIABLE TEST_RESULT
  )

  message (STATUS "COMPARE Result: ${TEST_RESULT}: ${TEST_STRING_SIZE}=${TEST_U_STRING_LEN}+${TEST_O_STRING_LEN}")
  # If the comparison fails, report an error
  if (NOT TEST_RESULT EQUAL TEST_EXPECT)
    message (FATAL_ERROR "Failed: The output of ${TEST_HFILE}-ub did not match ${TEST_HFILE}.\n${TEST_ERROR}")
  endif ()
else ()
  # --- User block size checking mode (no content check) ---
  # Run tellub to get the user block size of the test file
  execute_process (
      COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_HFILE}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_H_STRING_LEN
      OUTPUT_VARIABLE TEST_OUT
      ERROR_VARIABLE TEST_ERROR
  )
  # If the user block size is not zero, report an error
  if (TEST_H_STRING_LEN)
    message (FATAL_ERROR "Failed: The output of ${TEST_HFILE} was NOT empty")
  endif ()
endif ()

# If everything passed, print a success message
message (STATUS "Passed: The output of CHECK matched expectation")
