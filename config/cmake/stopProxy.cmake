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
# stopProxy.cmake - Script to stop and remove a docker instance of s3proxy
#
# This script stops and removes a running docker container for s3proxy, capturing
# output and error logs, and checks the exit status against the expected value.
# It also cleans up output files unless HDF5_NOCLEANUP is set in the environment.
#
# Required variables:
#   - TEST_PROGRAM: The docker command (e.g., 'docker')
#   - TEST_FOLDER: Directory where the command is run
#   - TEST_ARGS: Arguments to pass to the docker command (container name, etc.)
#   - TEST_EXPECT: Expected exit code
# -----------------------------------------------------------------------------
#
# stopProxy.cmake shuts down a docker instance of s3proxy.

# Check that required arguments are defined
if (NOT TEST_PROGRAM) # currently this is the docker command
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_FOLDER) # this is the folder where the test program is run
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()

# Inform user which s3proxy instance is being stopped
message (STATUS "Stopping s3proxy instance ${TEST_ARGS}")

# Run 'docker stop' to stop the s3proxy container, capturing output and error
execute_process (
    COMMAND ${TEST_PROGRAM} stop ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE docker-stop.out
    ERROR_FILE docker-stop.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# If the return value is not as expected, print output and fail
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (EXISTS "${TEST_FOLDER}/docker-stop.out")
    file (READ ${TEST_FOLDER}/docker-stop.out TEST_STREAM)
    message (STATUS "Output stopping s3proxy:\n${TEST_STREAM}")
  endif ()
  message (FATAL_ERROR "Failed: s3proxy exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# Run 'docker rm' to remove the s3proxy container, capturing output and error
execute_process (
    COMMAND ${TEST_PROGRAM} rm ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE docker-rm.out
    ERROR_FILE docker-rm.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# If the return value is not as expected, print output and fail
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (EXISTS "${TEST_FOLDER}/docker-stop.out")
    file (READ ${TEST_FOLDER}/docker-rm.out TEST_STREAM)
    message (STATUS "Output removing s3proxy:\n${TEST_STREAM}")
  endif ()
  message (FATAL_ERROR "Failed: s3proxy exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# Clean up the output files unless HDF5_NOCLEANUP is set in the environment
if (NOT DEFINED ENV{HDF5_NOCLEANUP})
  if (EXISTS "${TEST_FOLDER}/docker-stop.out")
    file (REMOVE ${TEST_FOLDER}/docker-stop.out)
  endif ()

  if (EXISTS "${TEST_FOLDER}/docker-stop.err")
    file (REMOVE ${TEST_FOLDER}/docker-stop.err)
  endif ()
  if (EXISTS "${TEST_FOLDER}/docker-rm.out")
    file (REMOVE ${TEST_FOLDER}/docker-rm.out)
  endif ()

  if (EXISTS "${TEST_FOLDER}/docker-rm.err")
    file (REMOVE ${TEST_FOLDER}/docker-rm.err)
  endif ()
endif ()

# Everything went fine...
message (STATUS "Passed: s3proxy instance ${TEST_ARGS} stopped.")
