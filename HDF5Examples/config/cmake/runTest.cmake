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
# runTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.
cmake_policy(SET CMP0007 NEW)
cmake_policy(SET CMP0053 NEW)

# arguments checking
if (NOT TEST_PROGRAM)
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_FOLDER)
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_OUTPUT)
  message (FATAL_ERROR "Require TEST_OUTPUT to be defined")
endif ()
if (NOT TEST_EXPECT)
  message (VERBOSE "Optional TEST_EXPECT is not defined")
endif ()

message (STATUS "ARGS: ${TEST_EMULATOR}/'${TEST_JAVA}' ${TEST_PROGRAM} ${TEST_ARGS}")

include (${CMAKE_CURRENT_LIST_DIR}/runExecute.cmake)

EXECUTE_TEST (TEST_FOLDER ${TEST_FOLDER}
               TEST_JAVA ${TEST_JAVA}
               TEST_PROGRAM ${TEST_PROGRAM}
               TEST_ARGS ${TEST_ARGS}
               TEST_EMULATOR ${TEST_EMULATOR}
               TEST_OUTPUT ${TEST_OUTPUT}
               TEST_EXPECT ${TEST_EXPECT}
               TEST_LIBRARY_DIRECTORY ${TEST_LIBRARY_DIRECTORY}
               TEST_ENV_VAR ${TEST_ENV_VAR}
               TEST_ENV_VALUE ${TEST_ENV_VALUE}
               TEST_INPUT ${TEST_INPUT}
               TEST_CLASSPATH ${TEST_CLASSPATH}
               TEST_NOERRDISPLAY ${TEST_NOERRDISPLAY}
)

FILTER_TEST (TEST_OUTPUT ${TEST_OUTPUT}
               TEST_FOLDER ${TEST_FOLDER}
               TEST_NO_DISPLAY ${TEST_NO_DISPLAY}
               TEST_REGEX ${TEST_REGEX}
               TEST_ERRREF ${TEST_ERRREF}
               TEST_REFERENCE ${TEST_REFERENCE}
               TEST_MATCH ${TEST_MATCH}
               TEST_MASK_ERROR ${TEST_MASK_ERROR}
               TEST_FILTER ${TEST_FILTER}
               TEST_FILTER_REPLACE ${TEST_FILTER_REPLACE}
               TEST_REF_FILTER ${TEST_REF_FILTER}
)

COMPARE_TEST (TEST_OUTPUT ${TEST_OUTPUT}
               TEST_FOLDER ${TEST_FOLDER}
               TEST_GREP_EXPECT ${TEST_GREP_EXPECT}
               TEST_REFERENCE ${TEST_REFERENCE}
               TEST_ERRREF ${TEST_ERRREF}
               TEST_SKIP_COMPARE ${TEST_SKIP_COMPARE}
               TEST_SORT_COMPARE ${TEST_SORT_COMPARE}
               TEST_GREP_COMPARE ${TEST_GREP_COMPARE}
               TEST_GREP_FILTER ${TEST_GREP_FILTER}
)

# dump the output unless nodisplay option is set
if (TEST_SKIP_COMPARE AND NOT TEST_NO_DISPLAY AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E echo ${TEST_STREAM}
      RESULT_VARIABLE TEST_RESULT
  )
endif ()

# Check if the output files should not be removed
if (NOT DEFINED ENV{HDF5_NOCLEANUP})
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}" AND NOT TEST_SAVE)
    file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
  endif ()

  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
    file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
  endif ()

  if (TEST_DELETE_LIST)
    foreach (dfile in ${TEST_DELETE_LIST})
      file (REMOVE ${dfile})
    endforeach ()
  endif ()
endif ()

# everything went fine...
message (STATUS "${TEST_PROGRAM} Passed")

