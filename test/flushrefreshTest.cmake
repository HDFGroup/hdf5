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
# runTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.
cmake_policy(SET CMP0007 NEW)

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
if (NOT PERL_SCRIPT)
  message (STATUS "Require PERL_BIN_DIR to be defined")
endif ()
if (NOT PERL_EXECUTABLE)
  message (STATUS "Require PERL_EXECUTABLE to be defined")
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
endif ()

message (STATUS "COMMAND: ${TEST_PROGRAM} ${TEST_ARGS}")

if (TEST_LIBRARY_DIRECTORY)
  if (WIN32 OR MINGW)
    set (ENV{PATH} "$ENV{PATH};${TEST_LIBRARY_DIRECTORY}")
  else ()
    set (ENV{LD_LIBRARY_PATH} "$ENV{LD_LIBRARY_PATH}:${TEST_LIBRARY_DIRECTORY}")
  endif ()
endif ()

if (TEST_ENV_VAR)
  set (ENV{${TEST_ENV_VAR}} "${TEST_ENV_VALUE}")
  #message (STATUS "ENV:${TEST_ENV_VAR}=$ENV{${TEST_ENV_VAR}}")
endif ()

message (STATUS "Background: ${PERL_EXECUTABLE} ${PERL_SCRIPT} ${TEST_PROGRAM}")
execute_process (
    COMMAND ${PERL_EXECUTABLE} ${PERL_SCRIPT} ${TEST_PROGRAM}
    RESULT_VARIABLE SCRIPT_RESULT
    ERROR_VARIABLE SCRIPT_ERR
    OUTPUT_VARIABLE SCRIPT_OUTPUT
    WORKING_DIRECTORY ${TEST_FOLDER}
)
message (STATUS "Background: ${SCRIPT_OUTPUT}")
if (SCRIPT_RESULT)
  message (FATAL_ERROR "Failed: The background script failed ${SCRIPT_RESULT}: ${SCRIPT_ERR}")
endif ()

set (verification_done "0")
while (verification_done LESS "1")
  message (STATUS "checking first stage:${TEST_FOLDER}/${TEST_ARGS1}")
  if (EXISTS "${TEST_FOLDER}/${TEST_ERR}")
    # Error exit script
    set (verification_done "3")
  elseif (EXISTS "${TEST_FOLDER}/${TEST_ARGS1}")
    file (STRINGS ${TEST_FOLDER}/${TEST_ARGS1} v1)
    list (LENGTH v1 len_v1)
    message (STATUS "v1:${v1} len_v1:${len_v1}")
    if (len_v1)
      list (GET v1 0 param1)
      list (GET v1 -1 param2)
    endif ()
    file (REMOVE ${TEST_FOLDER}/${TEST_ARGS1})
    message (STATUS "PARAM1:${param1} PARAM2:${param2}")

    if (param1 MATCHES "VERIFICATION_DONE")
      set (verification_done "1")
      file (WRITE ${TEST_FOLDER}/${TEST_ARGS2} "all flush verification complete")
      message (STATUS "write: ${TEST_FOLDER}/${TEST_ARGS2}")
    else ()
      message (STATUS "execute: ${TEST_PROGRAM} ${param1} ${param2}")
      execute_process (
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} ${TEST_PROGRAM} ${param1} ${param2}
          RESULT_VARIABLE TEST_RESULT
          OUTPUT_FILE ${TEST_OUTPUT}
          ERROR_FILE ${TEST_OUTPUT}.err
          OUTPUT_VARIABLE TEST_OUT
          ERROR_VARIABLE TEST_ERROR
          WORKING_DIRECTORY ${TEST_FOLDER}
      )
      message (STATUS "flush verification: ${TEST_OUT}")
      if (TEST_RESULT)
        message (FATAL_ERROR "Failed: The flush verification failed ${TEST_RESULT}: ${TEST_ERROR}")
      endif ()
      file (WRITE ${TEST_FOLDER}/${TEST_ARGS2} "verification flush process done")
    endif ()
  else ()
    message (STATUS "waiting: ${TEST_FOLDER}/${TEST_ARGS1}")
    #execute_process (COMMAND ${CMAKE_COMMAND} -E sleep 2)
  endif ()
endwhile ()

while (verification_done LESS "2")
  message (STATUS "checking second stage:${TEST_FOLDER}/${TEST_ARGS1}")
  if (EXISTS "${TEST_FOLDER}/${TEST_ERR}")
    # Error exit script
    set (verification_done "3")
  elseif (EXISTS "${TEST_FOLDER}/${TEST_ARGS1}")
    file (STRINGS ${TEST_FOLDER}/${TEST_ARGS1} v1)
    list (LENGTH v1 len_v1)
    message (STATUS "v1:${v1} len_v1:${len_v1}")
    if (len_v1)
      list (GET v1 0 param1)
      list (GET v1 -1 param2)
    endif ()
    file (REMOVE ${TEST_FOLDER}/${TEST_ARGS1})
    message (STATUS "PARAM1:${param1} PARAM2:${param2}")

    if (param1 MATCHES "VERIFICATION_DONE")
      set (verification_done "2")
      file (WRITE ${TEST_FOLDER}/${TEST_ARGS2} "all refresh verification complete")
      message (STATUS "write: ${TEST_FOLDER}/${TEST_ARGS2}")
    else ()
      message (STATUS "execute: ${TEST_PROGRAM} ${param1}")
      execute_process (
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} ${TEST_PROGRAM} ${param1}
          RESULT_VARIABLE TEST_RESULT
          OUTPUT_FILE ${TEST_OUTPUT}
          ERROR_FILE ${TEST_OUTPUT}.err
          OUTPUT_VARIABLE TEST_OUT
          ERROR_VARIABLE TEST_ERROR
          WORKING_DIRECTORY ${TEST_FOLDER}
      )
      message (STATUS "refresh verification: ${TEST_OUT}")
      if (TEST_RESULT)
        message (FATAL_ERROR "Failed: The refresh verification failed ${TEST_RESULT}: ${TEST_ERROR}")
      endif ()
      file (WRITE ${TEST_FOLDER}/${TEST_ARGS2} "refresh verifiction process done")
    endif ()
  else ()
    message (STATUS "waiting: ${TEST_FOLDER}/${TEST_ARGS1}")
   #execute_process (COMMAND ${CMAKE_COMMAND} -E sleep 2)
  endif ()
endwhile ()

message (STATUS "COMMAND Result: ${TEST_RESULT}")

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
      message (STATUS "Output :\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()

message (STATUS "COMMAND Error: ${TEST_ERROR}")

# everything went fine...
message (STATUS "Passed")
