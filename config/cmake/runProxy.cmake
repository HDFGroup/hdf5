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
# runProxy.cmake - Script to start a docker instance of s3proxy and upload files
#
# This script pulls the s3proxy docker image, starts a container, checks that it is running,
# creates a bucket, and uploads test files to the bucket. It captures output and error logs,
# checks exit status against the expected value, and cleans up output files unless
# HDF5_NOCLEANUP is set in the environment.
#
# Required variables:
#   - TEST_PROGRAM: The docker command (e.g., 'docker')
#   - TEST_PRODUCT: The docker image to use (e.g., 'gaul/s3proxy')
#   - TEST_PORT: The port to publish for the docker instance
#   - TEST_FOLDER: Directory where the command is run
#   - TEST_BUCKET: S3 bucket name to create and use
#   - TEST_ARGS: Arguments for docker container name, etc.
#   - TEST_EXPECT: Expected exit code
# Optional variables:
#   - TEST_ENV_VAR/TEST_ENV_VALUE: (optional) Environment variable to set
#   - TEST_FILES: (optional) List of files to upload
#   - TEST_ACLS: (optional) List of ACLs for files (anon/public-read)
#   - TEST_NOERRDISPLAY: (optional) If set, suppress error output display on failure
# -----------------------------------------------------------------------------

# arguments checking
if (NOT TEST_PROGRAM) # currently this is the docker command
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_PRODUCT) # this is the docker product to be used
  message (FATAL_ERROR "Require TEST_PRODUCT to be defined")
endif ()
if (NOT TEST_PORT) # this is the port for the docker instance
  message (FATAL_ERROR "Require TEST_PORT to be defined")
endif ()
if (NOT TEST_FOLDER) # this is the folder where the test program is run
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_BUCKET)
  message (FATAL_ERROR "Require TEST_BUCKET to be defined")
endif ()

# Optionally set an environment variable for the docker run
if (TEST_ENV_VAR)
  set (ENV{${TEST_ENV_VAR}} "${TEST_ENV_VALUE}")
  message (VERBOSE "ENV:${TEST_ENV_VAR}=$ENV{${TEST_ENV_VAR}}")
endif ()
message (STATUS "USING ${TEST_BUCKET} ON COMMAND: docker ${TEST_PRODUCT} ${TEST_ARGS} with creds $ENV{AWS_SHARED_CREDENTIALS_FILE}")

# Pull the docker image for s3proxy
execute_process (
    COMMAND ${TEST_PROGRAM} pull ${TEST_PRODUCT}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-pull.out
    ERROR_FILE s3proxy-pull.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (VERBOSE "COMMAND Pull Result: ${TEST_RESULT}")

# Start the s3proxy docker container with required environment variables and port mapping
execute_process (
    COMMAND ${TEST_PROGRAM} run -d --publish ${TEST_PORT}:80 --restart=always --name ${TEST_ARGS} --env S3PROXY_AUTHORIZATION=aws-v4 --env S3PROXY_ENDPOINT=http://0.0.0.0:80 --env S3PROXY_IDENTITY=remote-identity --env S3PROXY_CREDENTIAL=remote-credential --env S3PROXY_CORS_ALLOW_ALL=true ${TEST_PRODUCT}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-run.out
    ERROR_FILE s3proxy-run.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

# Print the output of the run command if it exists
if (EXISTS "${TEST_FOLDER}/s3proxy-run.out")
  file (READ ${TEST_FOLDER}/s3proxy-run.out TEST_STREAM)
  message (VERBOSE "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
endif ()
message (VERBOSE "COMMAND Run Result: ${TEST_RESULT}")

# If the return value is not as expected, print error output and fail
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/s3proxy-run.err")
      file (READ ${TEST_FOLDER}/s3proxy-run.err TEST_STREAM)
      message (STATUS "Error output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PRODUCT} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

# Wait for the container to be ready
execute_process(COMMAND ${CMAKE_COMMAND} -E sleep 10)

# Check that the docker instance is running
execute_process (
    COMMAND ${TEST_PROGRAM} inspect --format='{{.State.Running}}' ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-filter.out
    ERROR_FILE s3proxy-filter.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (VERBOSE "COMMAND Inspect Result: ${TEST_RESULT}")

# Check that the output contains 'true' (container is running)
if (NOT TEST_NOERRDISPLAY)
  if (EXISTS "${TEST_FOLDER}/s3proxy-filter.out")
    file (READ ${TEST_FOLDER}/s3proxy-filter.out TEST_STREAM)
    message (STATUS "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    string (REGEX MATCH "true" REGEX_MATCH ${TEST_STREAM})
    string (COMPARE EQUAL "${REGEX_MATCH}" "true" REGEX_RESULT)
    if (NOT REGEX_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_PROGRAM} did not contain true")
    endif ()
  endif ()
endif ()
# If the return value is not as expected, print error output and fail
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/s3proxy-filter.err")
      file (READ ${TEST_FOLDER}/s3proxy-filter.err TEST_STREAM)
      message (STATUS "Error output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PRODUCT} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

# Create the S3 bucket using AWS CLI
execute_process (
    COMMAND aws s3api create-bucket --endpoint-url=http://localhost:${TEST_PORT} --bucket ${TEST_BUCKET}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-bucket.out
    ERROR_FILE s3proxy-bucket.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

# Print the output of the bucket creation if it exists
if (EXISTS "${TEST_FOLDER}/s3proxy-bucket.out")
  file (READ ${TEST_FOLDER}/s3proxy-bucket.out TEST_STREAM)
  message (VERBOSE "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
endif ()
message (VERBOSE "COMMAND Bucket Result: ${TEST_RESULT}")

# If the return value is not as expected, print error output and fail
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/s3proxy-bucket.err")
      file (READ ${TEST_FOLDER}/s3proxy-bucket.err TEST_STREAM)
      message (STATUS "Error output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Create-Bucket exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

# Upload test files to the bucket, handling ACLs if provided
if (TEST_FILES AND TEST_ACLS)
  foreach (dfile dacls IN ZIP_LISTS TEST_FILES TEST_ACLS)
    if (dacls STREQUAL "anon")
        execute_process (
            COMMAND aws s3api put-object --acl public-read --endpoint-url=http://localhost:${TEST_PORT} --body ${TEST_FOLDER}/testfiles/${dfile} --bucket ${TEST_BUCKET} --key ${dfile}
            WORKING_DIRECTORY ${TEST_FOLDER}
            RESULT_VARIABLE TEST_RESULT
            OUTPUT_FILE s3proxy-${dfile}.out
            ERROR_FILE s3proxy-${dfile}.err
            OUTPUT_VARIABLE TEST_OUT
            ERROR_VARIABLE TEST_ERROR
        )
    else ()
        execute_process (
            COMMAND aws s3api put-object --endpoint-url=http://localhost:${TEST_PORT} --body ${TEST_FOLDER}/testfiles/${dfile} --bucket ${TEST_BUCKET} --key ${dfile}
            WORKING_DIRECTORY ${TEST_FOLDER}
            RESULT_VARIABLE TEST_RESULT
            OUTPUT_FILE s3proxy-${dfile}.out
            ERROR_FILE s3proxy-${dfile}.err
            OUTPUT_VARIABLE TEST_OUT
            ERROR_VARIABLE TEST_ERROR
        )
    endif ()
    # Print the output of the put-object command if it exists
    if (EXISTS "${TEST_FOLDER}/s3proxy-${dfile}.out")
      file (READ ${TEST_FOLDER}/s3proxy-${dfile}.out TEST_STREAM)
      message (VERBOSE "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
    message (VERBOSE "COMMAND Put Result: ${TEST_RESULT}")

    # If the return value is not as expected, print error output and fail
    if (NOT TEST_RESULT EQUAL TEST_EXPECT)
      if (NOT TEST_NOERRDISPLAY)
        if (EXISTS "${TEST_FOLDER}/s3proxy-${dfile}.err")
          file (READ ${TEST_FOLDER}/s3proxy-${dfile}.err TEST_STREAM)
          message (STATUS "Error output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
        endif ()
      endif ()
      message (FATAL_ERROR "Failed: Put-Object exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
    endif ()
  endforeach ()
elseif (TEST_FILES)
  foreach (dfile ${TEST_FILES})
    execute_process (
        COMMAND aws s3api put-object --endpoint-url=http://localhost:${TEST_PORT} --body ${TEST_FOLDER}/testfiles/${dfile} --bucket ${TEST_BUCKET} --key ${dfile}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE s3proxy-${dfile}.out
        ERROR_FILE s3proxy-${dfile}.err
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )
    # Print the output of the put-object command if it exists
    if (EXISTS "${TEST_FOLDER}/s3proxy-${dfile}.out")
      file (READ ${TEST_FOLDER}/s3proxy-${dfile}.out TEST_STREAM)
      message (VERBOSE "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
    message (VERBOSE "COMMAND Put Result: ${TEST_RESULT}")

    # If the return value is not as expected, print error output and fail
    if (NOT TEST_RESULT EQUAL TEST_EXPECT)
      if (NOT TEST_NOERRDISPLAY)
        if (EXISTS "${TEST_FOLDER}/s3proxy-${dfile}.err")
          file (READ ${TEST_FOLDER}/s3proxy-${dfile}.err TEST_STREAM)
          message (STATUS "Error output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
        endif ()
      endif ()
      message (FATAL_ERROR "Failed: Put-Object exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
    endif ()
  endforeach ()
endif ()

# Cleanup the output files unless HDF5_NOCLEANUP is set in the environment
if (NOT DEFINED ENV{HDF5_NOCLEANUP})
  file (GLOB REMOVE_FILES ${TEST_FOLDER}/s3proxy*)
  file (REMOVE ${REMOVE_FILES})
endif ()

# Everything went fine...
message (STATUS "Passed: The ${TEST_PRODUCT} docker used ${TEST_BUCKET}")
