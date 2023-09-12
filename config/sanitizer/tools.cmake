#
# Copyright (C) 2018-2020 by George Cave - gcave@stablecoder.ca
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

option(CLANG_TIDY "Turns on clang-tidy processing if it is found." OFF)
if(CLANG_TIDY)
  find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
  mark_as_advanced(FORCE CLANG_TIDY_EXE)
  if(CLANG_TIDY_EXE)
    message(STATUS "clang-tidy found: ${CLANG_TIDY_EXE}")
    set(CMAKE_C_CLANG_TIDY ${CLANG_TIDY_EXE} --checks=-*,clang-analyzer-*)#${ARGN})
    set(CMAKE_CXX_CLANG_TIDY ${CLANG_TIDY_EXE} --checks=-*,clang-analyzer-*)#${ARGN})
  else()
    message(STATUS "clang-tidy not found!")
    set(CMAKE_C_CLANG_TIDY "" CACHE STRING "" FORCE) # delete it
    set(CMAKE_CXX_CLANG_TIDY "" CACHE STRING "" FORCE) # delete it
  endif()
else()
  #message(STATUS "clang-tidy not enabled!")
  set(CMAKE_C_CLANG_TIDY
      ""
      CACHE STRING "" FORCE) # delete it
  set(CMAKE_CXX_CLANG_TIDY
      ""
      CACHE STRING "" FORCE) # delete it
endif()

# Adds clang-tidy checks to the compilation, with the given arguments being used
# as the options set.
macro(clang_tidy)
  if(CLANG_TIDY AND CLANG_TIDY_EXE)
    set(CMAKE_C_CLANG_TIDY ${CLANG_TIDY_EXE} ${ARGN})
    set(CMAKE_CXX_CLANG_TIDY ${CLANG_TIDY_EXE} ${ARGN})
  endif()
endmacro()

option(IWYU "Turns on include-what-you-use processing if it is found." OFF)
if(IWYU)
  find_program(IWYU_EXE NAMES "include-what-you-use")
  mark_as_advanced(FORCE IWYU_EXE)
  if(IWYU_EXE)
    message(STATUS "include-what-you-use found: ${IWYU_EXE}")
  else()
    message(SEND_ERROR "Cannot enable include-what-you-use, as executable not found!")
    set(CMAKE_C_INCLUDE_WHAT_YOU_USE "" CACHE STRING "" FORCE) # delete it
    set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE "" CACHE STRING "" FORCE) # delete it
  endif()
else()
  #message(STATUS "include-what-you-use NOT ENABLED via 'IWYU' variable!")
  set(CMAKE_C_INCLUDE_WHAT_YOU_USE "" CACHE STRING "" FORCE) # delete it
  set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE "" CACHE STRING "" FORCE) # delete it
endif()

# Adds include_what_you_use to the compilation, with the given arguments being
# used as the options set.
macro(include_what_you_use)
  if(IWYU AND IWYU_EXE)
    set(CMAKE_C_INCLUDE_WHAT_YOU_USE ${IWYU_EXE} ${ARGN})
    set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE ${IWYU_EXE} ${ARGN})
  endif()
endmacro()

option(CPPCHECK "Turns on cppcheck processing if it is found." OFF)
if(CPPCHECK)
  if(CPPCHECK_EXE)
    message(STATUS "cppcheck found: ${CPPCHECK_EXE}")
    set(CMAKE_C_CPPCHECK
        "${CPPCHECK_EXE};--enable=warning,performance,portability,missingInclude;--template=\"[{severity}][{id}] {message} {callstack} \(On {file}:{line}\)\";--suppress=missingIncludeSystem;--quiet;--verbose;--force"
    )
    set(CMAKE_CXX_CPPCHECK
        "${CPPCHECK_EXE};--enable=warning,performance,portability,missingInclude;--template=\"[{severity}][{id}] {message} {callstack} \(On {file}:{line}\)\";--suppress=missingIncludeSystem;--quiet;--verbose;--force"
    )
  else()
    message(SEND_ERROR "Cannot enable cppcheck, as executable not found!")
    set(CMAKE_C_CPPCHECK "" CACHE STRING "" FORCE) # delete it
    set(CMAKE_CXX_CPPCHECK "" CACHE STRING "" FORCE) # delete it
  endif()
else()
  #  message(SEND_ERROR "cppcheck NOT ENABLED via 'CPPCHECK' variable!")
  set(CMAKE_C_CPPCHECK "" CACHE STRING "" FORCE) # delete it
  set(CMAKE_CXX_CPPCHECK "" CACHE STRING "" FORCE) # delete it
endif()

# Adds cppcheck to the compilation, with the given arguments being used as the
# options set.
macro(cppcheck)
  if(CPPCHECK AND CPPCHECK_EXE)
    set(CMAKE_C_CPPCHECK ${CPPCHECK_EXE} ${ARGN})
    set(CMAKE_CXX_CPPCHECK ${CPPCHECK_EXE} ${ARGN})
  endif()
endmacro()

