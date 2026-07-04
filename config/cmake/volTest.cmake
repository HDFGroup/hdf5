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
# volTest.cmake - Test script for HDF5 Virtual Object Layer (VOL) functionality
#
# This script runs a test program using a specified VOL connector, captures its output and error,
# and compares the exit status to the expected value. It can also append error output
# to the main output if requested, and cleans up test files unless HDF5_NOCLEANUP is set.
#
# Required variables:
#   - TEST_PROGRAM: The test program to execute
#   - TEST_FOLDER: Directory containing test files
#   - TEST_VOL: Name of the VOL connector to use (sets HDF5_VOL_CONNECTOR)
#   - TEST_OUTPUT: Output file name (used for stdout/stderr)
#   - TEST_ARGS: Arguments to pass to the test program
#   - TEST_EXPECT: Expected exit code
# Optional variables:
#   - TEST_EMULATOR: (optional) Emulator to use (e.g., for cross-platform)
#   - ERROR_APPEND: (optional) If set, append error output to main output
#   - TEST_NOERRDISPLAY: (optional) If set, suppress error output display on failure
#   - TEST_DELETE_LIST: (optional) List of files to delete after test
# -----------------------------------------------------------------------------
#
# volTest.cmake executes a command and captures the output in a file. Command uses specified VOL.
# Exit status of command can also be compared.

# Check that all required arguments are defined
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_VOL)
  message (FATAL_ERROR "Require TEST_VOL to be defined")
endif ()

# Remove any existing output files before running the test
if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
endif ()

message (STATUS "USING ${TEST_VOL} ON COMMAND: ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}")

# Set the HDF5_VOL_CONNECTOR environment variable to the requested VOL
set (ENV{HDF5_VOL_CONNECTOR} "${TEST_VOL}")

# Run the test program, capturing stdout and stderr to separate files
execute_process (
    COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE ${TEST_OUTPUT}.out
    ERROR_FILE ${TEST_OUTPUT}.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# If ERROR_APPEND is enabled and the .err file exists, append error output to main output
if (ERROR_APPEND AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  file (APPEND ${TEST_FOLDER}/${TEST_OUTPUT}.out "${TEST_STREAM}")
endif ()

# If the return value is not as expected, display output and fail
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.out")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.out TEST_STREAM)
      message (STATUS "Output USING ${TEST_VOL}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# Clean up output files unless HDF5_NOCLEANUP is set in the environment
if (NOT DEFINED ENV{HDF5_NOCLEANUP})
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
  endif ()

  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
    file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
  endif ()

  # Optionally remove additional files listed in TEST_DELETE_LIST
  if (TEST_DELETE_LIST)
    foreach (dfile in ${TEST_DELETE_LIST})
      file (REMOVE ${dfile})
    endforeach ()
  endif ()
endif ()

# Everything went fine...
message (STATUS "Passed: The ${TEST_PROGRAM} program used vol ${TEST_VOL}")
