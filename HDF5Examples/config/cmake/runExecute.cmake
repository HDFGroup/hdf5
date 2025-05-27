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
# runExecute.cmake executes a command included by runTest.cmake.

macro (EXECUTE_TEST)
  cmake_parse_arguments (TEST "" "NOERRDISPLAY;EXPECT;JAVA;CLASSPATH;PROGRAM;FOLDER;OUTPUT;LIBRARY_DIRECTORY;INPUT;ENV_VAR;ENV_VALUE;EMULATOR;ARGS" "TEST_" ${ARGN})
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
if (NOT TEST_JAVA)
  message (VERBOSE "Optional TEST_JAVA is not defined")
endif ()
message (STATUS "EXECUTE ARGS: ${TEST_EMULATOR}/${TEST_JAVA} ${TEST_PROGRAM} ${TEST_ARGS}")

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT})
endif ()

if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (REMOVE ${TEST_FOLDER}/${TEST_OUTPUT}.err)
endif ()

if (TEST_LIBRARY_DIRECTORY) # Directory to add to PATH
  if (WIN32)
    set (ENV{PATH} "$ENV{PATH};${TEST_LIBRARY_DIRECTORY}")
  elseif (APPLE)
    set (ENV{DYLD_LIBRARY_PATH} "$ENV{DYLD_LIBRARY_PATH}:${TEST_LIBRARY_DIRECTORY}")
  else ()
    set (ENV{LD_LIBRARY_PATH} "$ENV{LD_LIBRARY_PATH}:${TEST_LIBRARY_DIRECTORY}")
  endif ()
endif ()

if (TEST_ENV_VAR)
  set (ENV{${TEST_ENV_VAR}} "${TEST_ENV_VALUE}")
  message (TRACE "ENV:${TEST_ENV_VAR}=$ENV{${TEST_ENV_VAR}}")
endif ()

if (NOT TEST_JAVA)
  message (STATUS "COMMAND: ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}")
  if (NOT TEST_INPUT)
    # run the test program, capture the stdout/stderr and the result var
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        OUTPUT_FILE ${TEST_OUTPUT}
        ERROR_FILE ${TEST_OUTPUT}.err
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )
  else ()
    # run the test program with stdin, capture the stdout/stderr and the result var
    execute_process (
        COMMAND ${TEST_EMULATOR} ${TEST_PROGRAM} ${TEST_ARGS}
        WORKING_DIRECTORY ${TEST_FOLDER}
        RESULT_VARIABLE TEST_RESULT
        INPUT_FILE ${TEST_INPUT}
        OUTPUT_FILE ${TEST_OUTPUT}
        ERROR_FILE ${TEST_OUTPUT}.err
        OUTPUT_VARIABLE TEST_OUT
        ERROR_VARIABLE TEST_ERROR
    )
  endif ()
else ()
  message (STATUS "JAVA COMMAND:  ${TEST_JAVA} -Xmx1024M -Dorg.slf4j.simpleLogger.defaultLogLevel=${LOG_LEVEL} -Djava.library.path=${TEST_LIBRARY_DIRECTORY} -cp \"${TEST_CLASSPATH}\" ${TEST_ARGS} ${TEST_PROGRAM}")
  # run the test program, capture the stdout/stderr and the result var
  execute_process (
      COMMAND ${TEST_JAVA} -Xmx1024M
      -Dorg.slf4j.simpleLogger.defaultLogLevel=${LOG_LEVEL}
      -Djava.library.path=${TEST_LIBRARY_DIRECTORY}
      -cp "${TEST_CLASSPATH}" ${TEST_ARGS} ${TEST_PROGRAM}
      WORKING_DIRECTORY ${TEST_FOLDER}
      RESULT_VARIABLE TEST_RESULT
      OUTPUT_FILE ${TEST_OUTPUT}
      ERROR_FILE ${TEST_OUTPUT}.err
      OUTPUT_VARIABLE TEST_OUT
      ERROR_VARIABLE TEST_ERROR
  )
endif ()

message (STATUS "COMMAND Result: ${TEST_RESULT}")
message (STATUS "COMMAND Output: ${TEST_OUT}")
message (STATUS "COMMAND Error: ${TEST_ERROR}")

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
      message (STATUS "Output :\n${TEST_STREAM}")
    endif ()
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
      message (STATUS "Error Output :\n${TEST_STREAM}")
    endif ()
  endif ()
  message (FATAL_ERROR "Failed: Test program ${TEST_PROGRAM} exited != ${TEST_EXPECT}.\n${TEST_ERROR}")
endif ()
endmacro ()

#############################################
# Begin of file filtering
#############################################
macro (FILTER_TEST)
cmake_parse_arguments (TEST "" "MASK_ERROR;MASK_STORE;ERRREF;FOLDER;OUTPUT;REFERENCE;REGEX;MATCH;MASK;FILTER;REF_FILTER;FILTER_REPLACE;MASK_MOD;ARGS" "TEST_" ${ARGN})
if (TEST_REGEX)
  # TEST_REGEX and TEST_MATCH should always be checked
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
    string (REGEX MATCH "${TEST_REGEX}" REGEX_MATCH ${TEST_STREAM})
    string (COMPARE EQUAL "${REGEX_MATCH}" "${TEST_MATCH}" REGEX_RESULT)
    if (NOT REGEX_RESULT)
      message (FATAL_ERROR "Failed: The output did not contain ${TEST_MATCH}")
    endif ()
  else ()
    message (STATUS "Failed: No output")
  endif ()
endif ()

# if the .err file exists
if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
  list (LENGTH TEST_STREAM test_len)
  if (test_len GREATER 0)
    # remove special output
    string (REGEX REPLACE "^.*_pmi_alps[^\n]+\n" "" TEST_STREAM "${TEST_STREAM}")
    string (FIND "${TEST_STREAM}" "no version information available" TEST_FIND_RESULT)
    if (TEST_FIND_RESULT GREATER -1)
      string (REGEX REPLACE "^.*no version information available[^\n]+\n" "" TEST_STREAM "${TEST_STREAM}")
      # write back the changes to the original files
      file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err "${TEST_STREAM}")
    endif ()

      # write back to original .err file
      file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err ${TEST_STREAM})
  endif ()
endif ()

# remove special regex text from the output
if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  string (FIND "${TEST_STREAM}" "_pmi_alps" TEST_FIND_RESULT)
  if (TEST_FIND_RESULT GREATER -1)
    string (REGEX REPLACE "^.*_pmi_alps[^\n]+\n" "" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_STREAM})
  endif ()
  string (FIND "${TEST_STREAM}" "ulimit -s" TEST_FIND_RESULT)
  if (TEST_FIND_RESULT GREATER -1)
    string (REGEX REPLACE "^.*ulimit -s[^\n]+\n" "" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_STREAM})
  endif ()
  string (FIND "${TEST_STREAM}" "no version information available" TEST_FIND_RESULT)
  if (TEST_FIND_RESULT GREATER -1)
    string (REGEX REPLACE "^.*no version information available[^\n]+\n" "" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  endif ()
  # if the output file needs Storage text masked out
  if (TEST_MASK_STORE)
    string (REGEX REPLACE "Storage:[^\n]+\n" "Storage:   <details removed for portability>\n" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  endif ()
  # if the output file needs Modified text removed
  if (TEST_MASK_MOD)
    string (REGEX REPLACE "Modified:[^\n]+\n" "Modified:  XXXX-XX-XX XX:XX:XX XXX\n" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  endif ()
endif ()

# if the output file or the .err file needs to mask out error stack info
if (TEST_MASK_ERROR)
  if (NOT TEST_ERRREF)
    # the error stack has been appended to the output file
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
    endif ()
  else ()
    # the error stack remains in the .err file
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
      file (READ ${TEST_FOLDER}/${TEST_OUTPUT}.err TEST_STREAM)
    endif ()
  endif ()
  string (REGEX REPLACE "Time:[^\n]+\n" "Time:  XXXX\n" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "thread [0-9]*:" "thread (IDs):" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE ": ([^\n]*)[.]c " ": (file name) " TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE " line [0-9]*" " line (number)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "v[1-9]*[.][0-9]*[.]" "version (number)." TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "HDF5 [1-9]*[.][0-9]*[.][0-9]*[^)]*" "version (number)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "H5Eget_auto[1-2]*" "H5Eget_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}")
  string (REGEX REPLACE "H5Eset_auto[1-2]*" "H5Eset_auto(1 or 2)" TEST_STREAM "${TEST_STREAM}")
  # write back the changes to the original files
  if (NOT TEST_ERRREF)
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_STREAM})
  else ()
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT}.err ${TEST_STREAM})
  endif ()
endif ()

# remove text from the output file
if (TEST_MASK)
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
    string (REGEX REPLACE "${TEST_MASK}" "" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  endif ()
endif ()

# replace text from the output file
if (TEST_FILTER)
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
    string (REGEX REPLACE "${TEST_FILTER}" "${TEST_FILTER_REPLACE}" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_OUTPUT} "${TEST_STREAM}")
  endif ()
endif ()

if (TEST_REF_FILTER)
  if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}")
    file (READ ${TEST_FOLDER}/${TEST_REFERENCE} TEST_STREAM)
    string (REGEX REPLACE "${TEST_REF_APPEND}" "${TEST_REF_FILTER}" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${TEST_FOLDER}/${TEST_REFERENCE} "${TEST_STREAM}")
  endif ()
endif ()
endmacro ()
#############################################
# End of file filtering
#############################################

macro (COMPARE_TEST)
cmake_parse_arguments (TEST "" "GREP_COMPARE;NO_DISPLAY;EXPECT;FOLDER;OUTPUT;REFERENCE;ERRREF;MATCH;SKIP_COMPARE;SORT_COMPARE;GREP_FILTER;ARGS" "TEST_" ${ARGN})
# compare output files to references unless this must be skipped
set (TEST_COMPARE_RESULT 0) # grep result variable; 0 is success
if (NOT TEST_SKIP_COMPARE)
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}")
      file (READ ${TEST_FOLDER}/${TEST_REFERENCE} TEST_STREAM)
      list (LENGTH TEST_STREAM test_len)
      # verify there is text output in the reference file
      if (test_len GREATER 0)
        if (NOT TEST_SORT_COMPARE)
          # now compare the output with the reference
          execute_process (
              COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${TEST_FOLDER}/${TEST_OUTPUT} ${TEST_FOLDER}/${TEST_REFERENCE}
              RESULT_VARIABLE TEST_COMPARE_RESULT
          )
        else () # sort the output files first before comparing
          file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} v1)
          file (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} v2)
          list (SORT v1)
          list (SORT v2)
          if (NOT v1 STREQUAL v2)
            set (TEST_COMPARE_RESULT 1)
          endif ()
        endif ()

        # only compare files if previous operations were successful
        if (TEST_COMPARE_RESULT)
          set (TEST_COMPARE_RESULT 0)
          file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT} test_act)
          list (LENGTH test_act len_act)
          file (STRINGS ${TEST_FOLDER}/${TEST_REFERENCE} test_ref)
          list (LENGTH test_ref len_ref)
          if (NOT len_act EQUAL len_ref)
            set (TEST_COMPARE_RESULT 1)
          endif ()
          if (len_act GREATER 0 AND len_ref GREATER 0)
            if (TEST_SORT_COMPARE)
              list (SORT test_act)
              list (SORT test_ref)
            endif ()
            math (EXPR _FP_LEN "${len_ref} - 1")
            foreach (line RANGE 0 ${_FP_LEN})
              if (line GREATER_EQUAL len_act)
                message (STATUS "COMPARE FAILED: ran out of lines in ${TEST_FOLDER}/${TEST_OUTPUT}")
                set (TEST_COMPARE_RESULT 1)
                break ()
              elseif (line GREATER_EQUAL len_ref)
                message (STATUS "COMPARE FAILED: ran out of lines in ${TEST_FOLDER}/${TEST_REFERENCE}")
                set (TEST_COMPARE_RESULT 1)
                break ()
              else ()
                list (GET test_act ${line} str_act)
                list (GET test_ref ${line} str_ref)
                if (NOT str_act STREQUAL str_ref)
                  if (str_act)
                    set (TEST_COMPARE_RESULT 1)
                    message (STATUS "line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
                  endif ()
                endif ()
              endif ()
            endforeach ()
          else () # len_act GREATER 0 AND len_ref GREATER 0
            if (len_act EQUAL 0)
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_OUTPUT} is empty")
            endif ()
            if (len_ref EQUAL 0)
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_REFERENCE} is empty")
            endif ()
          endif ()
        endif () # TEST_COMPARE_RESULT
      endif () # test_len GREATER 0
    endif () # EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}

    message (STATUS "COMPARE Result: ${TEST_COMPARE_RESULT}")

    # again, if return value is !=0 scream and shout
    if (TEST_COMPARE_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_OUTPUT} did not match ${TEST_REFERENCE}")
    endif ()
  else ()
    message (TRACE "Test output file ${TEST_FOLDER}/${TEST_OUTPUT} does not exist")
  endif ()

  # now compare the .err file with the error reference, if supplied
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
    set (TEST_ERRREF_RESULT 0)
    if (TEST_ERRREF AND EXISTS "${TEST_FOLDER}/${TEST_ERRREF}")
      file (READ ${TEST_FOLDER}/${TEST_ERRREF} TEST_STREAM)
      list (LENGTH TEST_STREAM test_len)
      if (test_len GREATER 0)
        # now compare the error output with the error reference
        execute_process (
            COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${TEST_FOLDER}/${TEST_OUTPUT}.err ${TEST_FOLDER}/${TEST_ERRREF}
            RESULT_VARIABLE TEST_ERRREF_RESULT
        )
        if (TEST_ERRREF_RESULT)
          set (TEST_ERRREF_RESULT 0)
          file (STRINGS ${TEST_FOLDER}/${TEST_OUTPUT}.err test_act)
          list (LENGTH test_act len_act)
          file (STRINGS ${TEST_FOLDER}/${TEST_ERRREF} test_ref)
          list (LENGTH test_ref len_ref)
          math (EXPR _FP_LEN "${len_ref} - 1")
          if (len_act GREATER 0 AND len_ref GREATER 0)
            math (EXPR _FP_LEN "${len_ref} - 1")
            foreach (line RANGE 0 ${_FP_LEN})
              list (GET test_act ${line} str_act)
              list (GET test_ref ${line} str_ref)
              if (NOT str_act STREQUAL str_ref)
                if (str_act)
                  set (TEST_ERRREF_RESULT 1)
                  message (STATUS "line = ${line}\n***ACTUAL: ${str_act}\n****REFER: ${str_ref}\n")
                endif ()
              endif ()
            endforeach ()
          else () # len_act GREATER 0 AND len_ref GREATER 0
            if (len_act EQUAL 0)
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_OUTPUT}.err is empty")
            endif ()
            if (len_ref EQUAL 0)
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_ERRREF} is empty")
            endif ()
          endif ()
          if (NOT len_act EQUAL len_ref)
            set (TEST_ERRREF_RESULT 1)
          endif ()
        endif () # TEST_ERRREF_RESULT
      endif () # test_len GREATER 0

      message (STATUS "COMPARE Result: ${TEST_ERRREF_RESULT}")

      # again, if return value is !=0 scream and shout
      if (TEST_ERRREF_RESULT)
        message (FATAL_ERROR "Failed: The error output of ${TEST_OUTPUT}.err did not match ${TEST_ERRREF}")
      endif ()
    endif () # TEST_ERRREF AND EXISTS "${TEST_FOLDER}/${TEST_ERRREF}
  else ()
    message (TRACE "Test output file ${TEST_FOLDER}/${TEST_OUTPUT}.err does not exist")
  endif ()
endif () # TEST_SKIP_COMPARE

set (TEST_GREP_RESULT 0)
if (TEST_GREP_COMPARE AND TEST_SKIP_COMPARE AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  # now grep the output with the reference
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  list (LENGTH TEST_STREAM test_len)
  if (test_len GREATER 0)
    # TEST_REFERENCE should always be matched
    string (REGEX MATCH "${TEST_REFERENCE}" REGEX_MATCH ${TEST_STREAM})
    string (COMPARE EQUAL "${TEST_REFERENCE}" "${REGEX_MATCH}" TEST_GREP_RESULT)
    if (NOT TEST_GREP_RESULT)
      message (FATAL_ERROR "Failed: The output did not contain ${TEST_REFERENCE}")
    endif ()
  endif ()
endif ()

# Check that TEST_GREP_FILTER text is not in the output when TEST_EXPECT is set to 1
if (TEST_GREP_FILTER AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  string (REGEX MATCH "${TEST_GREP_FILTER}" REGEX_MATCH ${TEST_STREAM})
  # TEST_EXPECT (1) interprets TEST_GREP_FILTER as; NOT to match
  if (TEST_EXPECT)
    string (LENGTH "${REGEX_MATCH}" TEST_GREP_RESULT)
    if (TEST_GREP_RESULT)
      message (FATAL_ERROR "Failed: The output did contain ${TEST_GREP_FILTER}")
    endif ()
  endif ()
endif ()

# dump the output unless nodisplay option is set
if (TEST_SKIP_COMPARE AND NOT TEST_NO_DISPLAY AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
  file (READ ${TEST_FOLDER}/${TEST_OUTPUT} TEST_STREAM)
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E echo ${TEST_STREAM}
      RESULT_VARIABLE TEST_RESULT
  )
endif ()
endmacro ()

