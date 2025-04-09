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
# runProxy.cmake dtarts a docker instance of s3proxy and uploads files.
# Exit status of command can also be compared.

# arguments checking
if (NOT TEST_PROGRAM) # currently this is the docker command
  message (FATAL_ERROR "Require TEST_PROGRAM to be defined")
endif ()
if (NOT TEST_PRODUCT) # this is the docker product to be used
  message (FATAL_ERROR "Require TEST_PRODUCT to be defined")
endif ()
if (NOT TEST_FOLDER) # this is the folder where the test program is run
  message (FATAL_ERROR "Require TEST_FOLDER to be defined")
endif ()
if (NOT TEST_BUCKET)
  message (FATAL_ERROR "Require TEST_BUCKET to be defined")
endif ()

message (STATUS "USING ${TEST_BUCKET} ON COMMAND: docker ${TEST_PRODUCT} ${TEST_ARGS}")

# run the test program to pull the product, capture the stdout/stderr and the result var
execute_process (
    COMMAND ${TEST_PROGRAM} pull ${TEST_PRODUCT}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-pull.out
    ERROR_FILE s3proxy-pull.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Pull Result: ${TEST_RESULT}")

# run the test program to start an instance of the product, capture the stdout/stderr and the result var
execute_process (
    COMMAND ${TEST_PROGRAM} run -d --publish 9001:80 --restart=always --name ${TEST_ARGS} --env S3PROXY_AUTHORIZATION=none --env S3PROXY_ENDPOINT=http://0.0.0.0:80 --env S3PROXY_IDENTITY=remote-identity --env S3PROXY_CREDENTIAL=remote-credential --env S3PROXY_CORS_ALLOW_ALL=true ${TEST_PRODUCT}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-run.out
    ERROR_FILE s3proxy-run.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Run Result: ${TEST_RESULT}")

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/s3proxy-run.out")
      file (READ ${TEST_FOLDER}/s3proxy-run.out TEST_STREAM)
      message (STATUS "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PRODUCT} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

# check that the docker instance is running
execute_process (
    COMMAND ${TEST_PROGRAM} ps --filter "name=${TEST_ARGS}" --filter "status=running"
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-filter.out
    ERROR_FILE s3proxy-filter.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Run Result: ${TEST_RESULT}")

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/s3proxy-run.out")
      file (READ ${TEST_FOLDER}/s3proxy-run.out TEST_STREAM)
      message (STATUS "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PRODUCT} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

# create the bucket to be used
execute_process (
    COMMAND aws s3api create-bucket --endpoint-url=http://localhost:9001 --bucket ${TEST_BUCKET}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE s3proxy-bucket.out
    ERROR_FILE s3proxy-bucket.err
    OUTPUT_VARIABLE TEST_OUT
    ERROR_VARIABLE TEST_ERROR
)

message (STATUS "COMMAND Bucket Result: ${TEST_RESULT}")

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/s3proxy-bucket.out")
      file (READ ${TEST_FOLDER}/s3proxy-bucket.out TEST_STREAM)
      message (STATUS "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Create-Bucket exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

#upload test files to the bucket
if (TEST_FILES)
  foreach (dfile ${TEST_FILES})
    execute_process (
        COMMAND aws s3api put-object --endpoint-url=http://localhost:9001 --body ${TEST_FOLDER}/testfiles/${dfile} --bucket ${TEST_BUCKET} --key ${dfile}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE s3proxy-${dfile}.out
        ERROR_FILE s3proxy-${dfile}.err
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )

    message (STATUS "COMMAND Put Result: ${TEST_RESULT}")

    # if the return value is !=${TEST_EXPECT} bail out
    if (NOT TEST_RESULT EQUAL TEST_EXPECT)
      if (NOT TEST_NOERRDISPLAY)
        if (EXISTS "${TEST_FOLDER}/s3proxy-${dfile}.out")
          file (READ ${TEST_FOLDER}/s3proxy-${dfile}.out TEST_STREAM)
          message (STATUS "Output USING ${TEST_BUCKET}:\n${TEST_STREAM}")
        endif ()
      endif ()
      message (FATAL_ERROR "Failed: Put-Object exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
    endif ()
  endforeach ()
endif ()

# cleanup the output files
if (NOT DEFINED ENV{HDF5_NOCLEANUP})
  file (GLOB REMOVE_FILES ${TEST_FOLDER}/s3proxy*)
  file (REMOVE ${REMOVE_FILES})
endif ()

# everything went fine...
message (STATUS "Passed: The ${TEST_PRODUCT} dockerm used ${TEST_BUCKET}")
