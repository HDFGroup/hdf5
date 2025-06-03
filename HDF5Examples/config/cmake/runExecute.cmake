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

macro(FILTER_FILE filename filters filters_replace)
    # avoid modifying external variables
    set(filename_int "${filename}")
    set(filters_int "${filters}")
    set(filters_replace_int "${filters_replace}")

    # read file
    file (READ ${filename_int} TEST_STREAM_INT)

    # perform the replacement
    list(LENGTH filters_int num_filters)
    # use loop index in order to detect potential empty strings in list
    math(EXPR filter_max_Idx "${num_filters}-1")

    foreach(i RANGE 0 ${filter_max_Idx})
        list(GET filters_int ${i} filter_i)
        if (NOT DEFINED filters_replace_int OR "${filters_replace_int}" STREQUAL "")
            set(filter_replace_i "")
        else()
            list(GET filters_replace_int ${i} filter_replace_i)
        endif()

        string (REGEX REPLACE "${filter_i}" "${filter_replace_i}" TEST_STREAM_INT "${TEST_STREAM_INT}")
    endforeach()

    # write the result to the original file
    file(WRITE "${filename_int}" "${TEST_STREAM_INT}")
endmacro()

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

# Set up filtered output/ref files, initially equal to the originals
if (DEFINED TEST_OUTPUT AND NOT "${TEST_OUTPUT}" STREQUAL "")
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}")
    file (READ "${TEST_FOLDER}/${TEST_OUTPUT}" TEST_STREAM)
    file (WRITE "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "${TEST_STREAM}")
  endif ()
endif ()

if (DEFINED TEST_REFERENCE AND NOT "${TEST_REFERENCE}" STREQUAL "")
  if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE}")
    file (READ "${TEST_FOLDER}/${TEST_REFERENCE}" TEST_STREAM)
    file (WRITE "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "${TEST_STREAM}")
  endif ()
endif ()

if (DEFINED TEST_OUTPUT AND NOT "${TEST_OUTPUT}" STREQUAL "")
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT}.err")
    file (READ "${TEST_FOLDER}/${TEST_OUTPUT}.err" TEST_STREAM)
    file (WRITE "${TEST_FOLDER}/${TEST_ERR_FILTERED}" "${TEST_STREAM}")
  endif ()
endif ()

if (DEFINED TEST_ERRREF AND NOT "${TEST_ERRREF}" STREQUAL "")
  if (EXISTS "${TEST_FOLDER}/${TEST_ERRREF}")
    file (READ "${TEST_FOLDER}/${TEST_ERRREF}" TEST_STREAM)
    file (WRITE "${TEST_FOLDER}/${TEST_ERRREF_FILTERED}" "${TEST_STREAM}")
  endif ()
endif ()

# if the return value is !=${TEST_EXPECT} bail out
if (NOT TEST_RESULT EQUAL TEST_EXPECT)
  if (NOT TEST_NOERRDISPLAY)
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
      file (READ "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" TEST_STREAM)
      message (STATUS "Output :\n${TEST_STREAM}")
    endif ()
    if (EXISTS "${TEST_FOLDER}/${TEST_ERR_FILTERED}")
      file (READ "${TEST_FOLDER}/${TEST_ERR_FILTERED}" TEST_STREAM)
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
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
    file (READ "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" TEST_STREAM)
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
if (EXISTS "${TEST_FOLDER}/${TEST_ERR_FILTERED}")
  file (READ "${TEST_FOLDER}/${TEST_ERR_FILTERED}" TEST_STREAM)
  list (LENGTH TEST_STREAM test_len)
  if (test_len GREATER 0)
    # remove special output
    FILTER_FILE("${TEST_FOLDER}/${TEST_ERR_FILTERED}" "^.*_pmi_alps[^\n]+\n" "")
    FILTER_FILE("${TEST_FOLDER}/${TEST_ERR_FILTERED}" "^.*no version information available[^\n]+\n" "")
    # apply same filtering to the reference file if it exists
    if (EXISTS "${TEST_FOLDER}/${TEST_ERRREF_FILTERED}")
      FILTER_FILE("${TEST_FOLDER}/${TEST_ERRREF_FILTERED}" "^.*_pmi_alps[^\n]+\n" "")
      FILTER_FILE("${TEST_FOLDER}/${TEST_ERRREF_FILTERED}" "^.*no version information available[^\n]+\n" "")
    endif ()
  endif ()
endif ()

# remove special regex text from the output
if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
  FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "^.*_pmi_alps[^\n]+\n" "")
  FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "^.*ulimit -s[^\n]+\n" "")
  FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "^.*no version information available[^\n]+\n" "")

  # mask out storage/modified text since it is not guaranteed to be consistent
  FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "Storage:[^\n]+\n" "Storage:   <details removed for portability>\n")
  FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "Modified:[^\n]+\n" "Modified:  XXXX-XX-XX XX:XX:XX XXX\n")
endif ()

# remove special regex text from the output reference
if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}")
  FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "^.*_pmi_alps[^\n]+\n" "")
  FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "^.*ulimit -s[^\n]+\n" "")
  FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "^.*no version information available[^\n]+\n" "")

  # mask out storage/modified text since it is not guaranteed to be consistent
  FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "Storage:[^\n]+\n" "Storage:   <details removed for portability>\n")
  FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "Modified:[^\n]+\n" "Modified:  XXXX-XX-XX XX:XX:XX XXX\n")
endif ()

set(TEST_MASKS_ERROR "")
list (APPEND TEST_MASKS_ERROR "Time:[^\n]+\n")
list (APPEND TEST_MASKS_ERROR "thread [0-9]*:")
list (APPEND TEST_MASKS_ERROR ": ([^\n]*)[.]c ")
list (APPEND TEST_MASKS_ERROR " line [0-9]*")
list (APPEND TEST_MASKS_ERROR "v[1-9]*[.][0-9]*[.]")
list (APPEND TEST_MASKS_ERROR "HDF5 [1-9]*[.][0-9]*[.][0-9]*[^)]*")
list (APPEND TEST_MASKS_ERROR "H5Eget_auto[1-2]*")
list (APPEND TEST_MASKS_ERROR "H5Eset_auto[1-2]*")

set(TEST_MASKS_ERROR_REPLACE "")
list (APPEND TEST_MASKS_ERROR_REPLACE "Time:  XXXX\n")
list (APPEND TEST_MASKS_ERROR_REPLACE "thread (IDs):")
list (APPEND TEST_MASKS_ERROR_REPLACE ": (file name) ")
list (APPEND TEST_MASKS_ERROR_REPLACE " line (number)")
list (APPEND TEST_MASKS_ERROR_REPLACE "version (number).")
list (APPEND TEST_MASKS_ERROR_REPLACE "version (number)")
list (APPEND TEST_MASKS_ERROR_REPLACE "H5Eget_auto(1 or 2)")
list (APPEND TEST_MASKS_ERROR_REPLACE "H5Eset_auto(1 or 2)")

# if the output file or the .err file needs to mask out error stack info
if (TEST_MASK_ERROR)
  if (NOT TEST_ERRREF)
    # the error stack has been appended to the output file
    if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
      FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "${TEST_MASKS_ERROR}" "${TEST_MASKS_ERROR_REPLACE}")
    endif ()
  else ()
    # the error stack remains in the .err file
    if (EXISTS "${TEST_FOLDER}/${TEST_ERR_FILTERED}")
      FILTER_FILE("${TEST_FOLDER}/${TEST_ERR_FILTERED}" "${TEST_MASKS_ERROR}" "${TEST_MASKS_ERROR_REPLACE}")
    endif ()
  endif ()
endif ()

# if the output reference file or the .err ref file needs to mask out error stack info
if (TEST_MASK_ERROR)
  if (NOT TEST_ERRREF)
    # the error stack has been appended to the output file
    if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}")
      FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "${TEST_MASKS_ERROR}" "${TEST_MASKS_ERROR_REPLACE}")
    endif ()
  else ()
    # the error stack remains in the .err file
    if (EXISTS "${TEST_FOLDER}/${TEST_ERRREF_FILTERED}")
      FILTER_FILE("${TEST_FOLDER}/${TEST_ERRREF_FILTERED}" "${TEST_MASKS_ERROR}" "${TEST_MASKS_ERROR_REPLACE}")
    endif ()
  endif ()
endif ()

# replace text from the output file
if (TEST_FILTER)
  # re-escape any backslashes to deal with being passed to macro
  string(REGEX REPLACE "([\\])" "\\\\\\1" TEST_FILTER "${TEST_FILTER}")
  string(REGEX REPLACE "([\\])" "\\\\\\1" TEST_FILTER_REPLACE "${TEST_FILTER_REPLACE}")

  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
    FILTER_FILE("${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "${TEST_FILTER}" "${TEST_FILTER_REPLACE}")
  endif ()

  # replace text from the output reference file
  if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}")
    FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "${TEST_FILTER}" "${TEST_FILTER_REPLACE}")
  endif ()
endif ()

if (TEST_REF_FILTER)
  if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}")
    # re-escape any backslashes
    string(REGEX REPLACE "([\\])" "\\\\\\1" TEST_REF_APPEND "${TEST_REF_APPEND}")
    string(REGEX REPLACE "([\\])" "\\\\\\1" TEST_REF_FILTER "${TEST_REF_FILTER}")
    # append text to the output reference file
    FILTER_FILE("${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" "${TEST_REF_APPEND}" "${TEST_REF_FILTER}")
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
  if (EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
    if (EXISTS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}")
      file (READ "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" TEST_STREAM)
      list (LENGTH TEST_STREAM test_len)
      # verify there is text output in the reference file
      if (test_len GREATER 0)
        if (NOT TEST_SORT_COMPARE)
          # now compare the output with the reference
          execute_process (
              COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}"
              RESULT_VARIABLE TEST_COMPARE_RESULT
          )
        else () # sort the output files first before comparing
          file (STRINGS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" v1)
          file (STRINGS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" v2)
          list (SORT v1)
          list (SORT v2)
          if (NOT v1 STREQUAL v2)
            set (TEST_COMPARE_RESULT 1)
          endif ()
        endif ()

        # only compare files if previous operations were successful
        if (TEST_COMPARE_RESULT)
          set (TEST_COMPARE_RESULT 0)
          file (STRINGS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" test_act)
          list (LENGTH test_act len_act)
          file (STRINGS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}" test_ref)
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
                message (STATUS "COMPARE FAILED: ran out of lines in ${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
                set (TEST_COMPARE_RESULT 1)
                break ()
              elseif (line GREATER_EQUAL len_ref)
                message (STATUS "COMPARE FAILED: ran out of lines in ${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}")
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
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_OUTPUT_FILTERED} is empty")
            endif ()
            if (len_ref EQUAL 0)
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_REFERENCE_FILTERED} is empty")
            endif ()
          endif ()
        endif () # TEST_COMPARE_RESULT
      endif () # test_len GREATER 0
    endif () # EXISTS "${TEST_FOLDER}/${TEST_REFERENCE_FILTERED}

    message (STATUS "COMPARE Result: ${TEST_COMPARE_RESULT}")

    # again, if return value is !=0 scream and shout
    if (TEST_COMPARE_RESULT)
      message (FATAL_ERROR "Failed: The output of ${TEST_OUTPUT_FILTERED} did not match ${TEST_REFERENCE_FILTERED}")
    endif ()
  else ()
    message (TRACE "Test output file ${TEST_FOLDER}/${TEST_OUTPUT_FILTERED} does not exist")
  endif ()

  # now compare the .err file with the error reference, if supplied
  if (EXISTS "${TEST_FOLDER}/${TEST_ERR_FILTERED}")
    set (TEST_ERRREF_RESULT 0)
    if (TEST_ERRREF AND EXISTS "${TEST_FOLDER}/${TEST_ERRREF_FILTERED}")
      file (READ ${TEST_FOLDER}/"${TEST_FOLDER}/${TEST_ERRREF_FILTERED}" TEST_STREAM)
      list (LENGTH TEST_STREAM test_len)
      if (test_len GREATER 0)
        # now compare the error output with the error reference
        execute_process (
            COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol "${TEST_FOLDER}/${TEST_ERR_FILTERED}" ${TEST_FOLDER}/${TEST_ERRREF_FILTERED}
            RESULT_VARIABLE TEST_ERRREF_RESULT
        )
        if (TEST_ERRREF_RESULT)
          set (TEST_ERRREF_RESULT 0)
          file (STRINGS "${TEST_FOLDER}/${TEST_ERR_FILTERED}" test_act)
          list (LENGTH test_act len_act)
          file (STRINGS ${TEST_FOLDER}/"${TEST_FOLDER}/${TEST_ERRREF_FILTERED}" test_ref)
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
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_ERR_FILTERED} is empty")
            endif ()
            if (len_ref EQUAL 0)
              message (STATUS "COMPARE Failed: ${TEST_FOLDER}/${TEST_ERRREF_FILTERED} is empty")
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
        message (FATAL_ERROR "Failed: The error output of ${TEST_FOLDER}/${TEST_ERR_FILTERED} did not match ${TEST_FOLDER}/${TEST_ERRREF_FILTERED}")
      endif ()
    endif () # TEST_ERRREF AND EXISTS "${TEST_FOLDER}/${TEST_ERRREF_FILTERED}
  else ()
    message (TRACE "Test output file ${TEST_FOLDER}/${TEST_ERR_FILTERED} does not exist")
  endif ()
endif () # TEST_SKIP_COMPARE

set (TEST_GREP_RESULT 0)
if (TEST_GREP_COMPARE AND TEST_SKIP_COMPARE AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
  # now grep the output with the reference
  # TBD: This section uses TEST_REFERENCE as a string to match, while other places
  # use it as a reference filename. For consistency, this case should eventually
  # be moved to a distinct argument.
  file (READ "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" TEST_STREAM)
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
if (TEST_GREP_FILTER AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
  file (READ "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" TEST_STREAM)
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
if (TEST_SKIP_COMPARE AND NOT TEST_NO_DISPLAY AND EXISTS "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}")
  file (READ "${TEST_FOLDER}/${TEST_OUTPUT_FILTERED}" TEST_STREAM)
  execute_process (
      COMMAND ${CMAKE_COMMAND} -E echo ${TEST_STREAM}
      RESULT_VARIABLE TEST_RESULT
  )
endif ()
endmacro ()

