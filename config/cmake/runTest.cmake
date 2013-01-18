# runTest.cmake executes a command and captures the output in a file. File is then compared
# against a reference file. Exit status of command can also be compared.
cmake_policy(SET CMP0007 NEW)

# arguments checking
IF (NOT TEST_PROGRAM)
  MESSAGE (FATAL_ERROR "Require TEST_PROGRAM to be defined")
ENDIF (NOT TEST_PROGRAM)
#IF (NOT TEST_ARGS)
#  MESSAGE (STATUS "Require TEST_ARGS to be defined")
#ENDIF (NOT TEST_ARGS)
IF (NOT TEST_FOLDER)
  MESSAGE ( FATAL_ERROR "Require TEST_FOLDER to be defined")
ENDIF (NOT TEST_FOLDER)
IF (NOT TEST_OUTPUT)
  MESSAGE (FATAL_ERROR "Require TEST_OUTPUT to be defined")
ENDIF (NOT TEST_OUTPUT)
IF (NOT TEST_EXPECT)
  MESSAGE (STATUS "Require TEST_EXPECT to be defined")
ENDIF (NOT TEST_EXPECT)
#IF (NOT TEST_FILTER)
#  MESSAGE (STATUS "Require TEST_FILTER to be defined")
#ENDIF (NOT TEST_FILTER)
IF (NOT TEST_SKIP_COMPARE AND NOT TEST_REFERENCE)
  MESSAGE (FATAL_ERROR "Require TEST_REFERENCE to be defined")
ENDIF (NOT TEST_SKIP_COMPARE AND NOT TEST_REFERENCE)

IF (NOT TEST_ERRREF)
  SET (ERROR_APPEND 1)
ENDIF (NOT TEST_ERRREF)

MESSAGE (STATUS "COMMAND: ${TEST_PROGRAM} ${TEST_ARGS}")

IF (TEST_ENV_VAR)
  SET (ENV{${TEST_ENV_VAR}} "${TEST_ENV_VALUE}") 
ENDIF (TEST_ENV_VAR)

# run the test program, capture the stdout/stderr and the result var
EXECUTE_PROCESS (
    COMMAND ${TEST_PROGRAM} ${TEST_ARGS}
    WORKING_DIRECTORY ${TEST_FOLDER}
    RESULT_VARIABLE TEST_RESULT
    OUTPUT_FILE ${TEST_OUTPUT}
    ERROR_FILE ${TEST_OUTPUT}.err
    OUTPUT_VARIABLE TEST_ERROR
    ERROR_VARIABLE TEST_ERROR
)

MESSAGE (STATUS "COMMAND Result: ${TEST_RESULT}")

IF (ERROR_APPEND)
  FILE (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  FILE (APPEND ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}") 
ENDIF (ERROR_APPEND)

IF (TEST_APPEND)
  FILE (APPEND ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_APPEND} ${TEST_RESULT}\n") 
ENDIF (TEST_APPEND)

# if the return value is !=${TEST_EXPECT} bail out
IF (NOT ${TEST_RESULT} STREQUAL ${TEST_EXPECT})
  MESSAGE ( FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
ENDIF (NOT ${TEST_RESULT} STREQUAL ${TEST_EXPECT})

MESSAGE (STATUS "COMMAND Error: ${TEST_ERROR}")

IF (TEST_MASK)
  FILE (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  STRING(REGEX REPLACE "Storage:[^\n]+\n" "Storage:   <details removed for portability>\n" TEST_STREAM "${TEST_STREAM}") 
  FILE (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
ENDIF (TEST_MASK)

IF (TEST_MASK_MOD)
  FILE (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  STRING(REGEX REPLACE "Modified:[^\n]+\n" "Modified:  XXXX-XX-XX XX:XX:XX XXX\n" TEST_STREAM "${TEST_STREAM}") 
  FILE (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
ENDIF (TEST_MASK_MOD)

IF (TEST_MASK_ERROR)
  IF (NOT TEST_ERRREF)
    FILE (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  ELSE (NOT TEST_ERRREF)
    FILE (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  ENDIF (NOT TEST_ERRREF)
  STRING(REGEX REPLACE "thread [0-9]*:" "thread (IDs):" TEST_STREAM "${TEST_STREAM}") 
  STRING(REGEX REPLACE ": ([^\n]*)[.]c " ": (file name) " TEST_STREAM "${TEST_STREAM}") 
  STRING(REGEX REPLACE " line [0-9]*" " line (number)" TEST_STREAM "${TEST_STREAM}") 
  STRING(REGEX REPLACE "v[1-9]*[.][0-9]*[.]" "version (number)." TEST_STREAM "${TEST_STREAM}") 
  STRING(REGEX REPLACE "[1-9]*[.][0-9]*[.][0-9]*[^)]*" "version (number)" TEST_STREAM "${TEST_STREAM}") 
  STRING(REGEX REPLACE "H5Eget_auto[1-2]*" "H5Eget_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}") 
  STRING(REGEX REPLACE "H5Eset_auto[1-2]*" "H5Eset_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}") 
  IF (NOT TEST_ERRREF)
    FILE (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  ELSE (NOT TEST_ERRREF)
    FILE (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err "${TEST_STREAM}")
  ENDIF (NOT TEST_ERRREF)
ENDIF (TEST_MASK_ERROR)

IF (TEST_FILTER)
  FILE (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  STRING(REGEX REPLACE "${TEST_FILTER}" "" TEST_STREAM "${TEST_STREAM}") 
  FILE (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
ENDIF (TEST_FILTER)

IF (NOT TEST_SKIP_COMPARE)
  IF (WIN32 AND NOT MINGW)
    FILE (READ ${TEST_FOLDER}/${TEST_REFERENCE} TEST_STREAM)
    FILE (WRITE ${TEST_FOLDER}/${TEST_REFERENCE} "${TEST_STREAM}")
  ENDIF (WIN32 AND NOT MINGW)

  # now compare the output with the reference
  EXECUTE_PROCESS (
      COMMAND ${CMAKE_COMMAND} -E compare_files ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_FOLDER}/${TEST_REFERENCE}
      RESULT_VARIABLE TEST_RESULT
  )
  IF (NOT ${TEST_RESULT} STREQUAL 0)
  SET (TEST_RESULT 0)
  FILE (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} test_act)
  LIST (LENGTH test_act len_act)
  FILE (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} test_ref)
  LIST (LENGTH test_ref len_ref)
  IF (NOT ${len_act} STREQUAL "0")
    MATH (EXPR _FP_LEN "${len_ref} - 1")
    FOREACH (line RANGE 0 ${_FP_LEN})
      LIST (GET test_act ${line} str_act)
      LIST (GET test_ref ${line} str_ref)
      IF (NOT "${str_act}" STREQUAL "${str_ref}")
        IF (NOT "${str_act}" STREQUAL "")
          SET (TEST_RESULT 1)
          MESSAGE ("line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
         ENDIF (NOT "${str_act}" STREQUAL "")
      ENDIF (NOT "${str_act}" STREQUAL "${str_ref}")
    ENDFOREACH (line RANGE 0 ${_FP_LEN})
  ENDIF (NOT ${len_act} STREQUAL "0")
  IF (NOT ${len_act} STREQUAL ${len_ref})
    SET (TEST_RESULT 1)
  ENDIF (NOT ${len_act} STREQUAL ${len_ref})
  ENDIF (NOT ${TEST_RESULT} STREQUAL 0)

  MESSAGE (STATUS "COMPARE Result: ${TEST_RESULT}")

  # again, if return value is !=0 scream and shout
  IF (NOT ${TEST_RESULT} STREQUAL 0)
    MESSAGE (FATAL_ERROR "Failed: The output of ${TEST_OUTPUT} did not match ${TEST_REFERENCE}")
  ENDIF (NOT ${TEST_RESULT} STREQUAL 0)
  
  IF (TEST_ERRREF)
    IF (WIN32 AND NOT MINGW)
      FILE (READ ${TEST_FOLDER}/${TEST_ERRREF} TEST_STREAM)
      FILE (WRITE ${TEST_FOLDER}/${TEST_ERRREF} "${TEST_STREAM}")
    ENDIF (WIN32 AND NOT MINGW)

    # now compare the error output with the error reference
    EXECUTE_PROCESS (
        COMMAND ${CMAKE_COMMAND} -E compare_files ${TEST_FOLDER}/${TEST_OUTPUT}.err ${TEST_FOLDER}/${TEST_ERRREF}
        RESULT_VARIABLE TEST_RESULT
    )
    IF (NOT ${TEST_RESULT} STREQUAL 0)
    SET (TEST_RESULT 0)
    FILE (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT}.err test_act)
    LIST (LENGTH test_act len_act)
    FILE (STRINGS ${TEST_FOLDER}/${TEST_ERRREF} test_ref)
    LIST (LENGTH test_ref len_ref)
    MATH (EXPR _FP_LEN "${len_ref} - 1")
    IF (NOT ${len_act} STREQUAL "0")
      MATH (EXPR _FP_LEN "${len_ref} - 1")
      FOREACH (line RANGE 0 ${_FP_LEN})
        LIST (GET test_act ${line} str_act)
        LIST (GET test_ref ${line} str_ref)
        IF (NOT "${str_act}" STREQUAL "${str_ref}")
          IF (NOT "${str_act}" STREQUAL "")
            SET (TEST_RESULT 1)
            MESSAGE ("line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
           ENDIF (NOT "${str_act}" STREQUAL "")
        ENDIF (NOT "${str_act}" STREQUAL "${str_ref}")
      ENDFOREACH (line RANGE 0 ${_FP_LEN})
    ENDIF (NOT ${len_act} STREQUAL "0")
    IF (NOT ${len_act} STREQUAL ${len_ref})
      SET (TEST_RESULT 1)
    ENDIF (NOT ${len_act} STREQUAL ${len_ref})
    ENDIF (NOT ${TEST_RESULT} STREQUAL 0)

    MESSAGE (STATUS "COMPARE Result: ${TEST_RESULT}")

    # again, if return value is !=0 scream and shout
    IF (NOT ${TEST_RESULT} STREQUAL 0)
      MESSAGE (FATAL_ERROR "Failed: The error output of ${TEST_OUTPUT}.err did not match ${TEST_ERRREF}")
    ENDIF (NOT ${TEST_RESULT} STREQUAL 0)
  ENDIF (TEST_ERRREF)
ENDIF (NOT TEST_SKIP_COMPARE)

# everything went fine...
MESSAGE ("Passed: The output of ${TEST_PROGRAM} matches ${TEST_REFERENCE}")
