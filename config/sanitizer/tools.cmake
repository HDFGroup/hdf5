#
# Copyright (C) 2018-2023 by George Cave - gcave@stablecoder.ca
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

# CLANG-TIDY
find_program(CLANG_TIDY_EXE NAMES "clang-tidy")
set(CLANG_TIDY_MESSAGE_OUTPUT # Control output messages to occur only once
    FALSE
    CACHE INTERNAL FALSE)
mark_as_advanced(FORCE CLANG_TIDY_EXE CMAKE_C_CLANG_TIDY CMAKE_CXX_CLANG_TIDY)

# Adds clang-tidy to code compiled after this macro. All arguments are added to
# the clang-tidy application call in the form of `clang-tidy ${ARGN}`.
#
# If the clang-tidy application is not found, the macro will cause CMake to
# produce an error and not generate.
#
# Options provided can be changed by calling the macro again with the new
# arguments.
macro(clang_tidy)
  # Only want to output whether clang-tidy was found once
  if(NOT CLANG_TIDY_MESSAGE_OUTPUT)
    set(CLANG_TIDY_MESSAGE_OUTPUT TRUE)
    if(CLANG_TIDY_EXE)
      message(STATUS "clang-tidy found: ${CLANG_TIDY_EXE}")
    else()
      message(SEND_ERROR "clang-tidy not found!")
    endif()
  endif()

  # Only pass the options if the tool was found
  if(CLANG_TIDY_EXE)
    set(CMAKE_C_CLANG_TIDY
        ${CLANG_TIDY_EXE} ${ARGN}
        CACHE STRING "" FORCE)
    set(CMAKE_CXX_CLANG_TIDY
        ${CLANG_TIDY_EXE} ${ARGN}
        CACHE STRING "" FORCE)
  endif()
endmacro()

# Clears clang-tidy so it is not called on any following defined code
# compilation. clang-tidy can be re-enabled by another call to `clang_tidy()`.
macro(reset_clang_tidy)
  set(CMAKE_C_CLANG_TIDY
      ""
      CACHE STRING "" FORCE)
  set(CMAKE_CXX_CLANG_TIDY
      ""
      CACHE STRING "" FORCE)
endmacro()

# INCLUDE-WHAT-YOU-USE
find_program(IWYU_EXE NAMES "include-what-you-use")
set(IWYU_MESSAGE_OUTPUT # Control output messages to occur only once
    FALSE
    CACHE INTERNAL FALSE)
mark_as_advanced(FORCE IWYU_EXE CMAKE_C_INCLUDE_WHAT_YOU_USE
                 CMAKE_CXX_INCLUDE_WHAT_YOU_USE)

# Adds include-what-you-use to code compiled after this macro. All arguments are
# added to the include-what-you-use application call in the form of
# `include-what-you-use ${ARGN}`.
#
# If the include-what-you-use application is not found, the macro will cause
# CMake to produce an error and not generate.
#
# Options provided can be changed by calling the macro again with the new
# arguments.
macro(include_what_you_use)
  # Only want to output whether clang-tidy was found once
  if(NOT IWYU_MESSAGE_OUTPUT)
    set(IWYU_MESSAGE_OUTPUT TRUE)
    if(IWYU_EXE)
      message(STATUS "include-what-you-use found: ${IWYU_EXE}")
    else()
      message(SEND_ERROR "include-what-you-use not found!")
    endif()
  endif()

  # Only pass the options if the tool was found
  if(IWYU_EXE)
    set(CMAKE_C_INCLUDE_WHAT_YOU_USE
        ${IWYU_EXE} ${ARGN}
        CACHE STRING "" FORCE)
    set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
        ${IWYU_EXE} ${ARGN}
        CACHE STRING "" FORCE)
  endif()
endmacro()

# Clears include-what-you-use so it is not called on any following defined code
# compilation. It can be re-enabled by another call to `include_what_you_use()`.
macro(reset_include_what_you_use)
  set(CMAKE_C_INCLUDE_WHAT_YOU_USE
      ""
      CACHE STRING "" FORCE)
  set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE
      ""
      CACHE STRING "" FORCE)
endmacro()

# CPPCHECK
find_program(CPPCHECK_EXE NAMES "cppcheck")
set(CPPCHECK_MESSAGE_OUTPUT # Control output messages to occur only once
    FALSE
    CACHE INTERNAL FALSE)
mark_as_advanced(FORCE CPPCHECK_EXE CMAKE_C_CPPCHECK CMAKE_CXX_CPPCHECK)

# Adds cppcheck to code compiled after this macro. All arguments are added to
# the cppcheck application call in the form of `cppcheck ${ARGN}`.
#
# If the include-what-you-use application is not found, the macro will cause
# CMake to produce an error and not generate.
#
# Options provided can be changed by calling the macro again with the new
# arguments.
macro(cppcheck)
  # Only want to output whether clang-tidy was found once
  if(NOT CPPCHECK_MESSAGE_OUTPUT)
    set(CPPCHECK_MESSAGE_OUTPUT TRUE)
    if(CPPCHECK_EXE)
      message(STATUS "cppcheck found: ${CPPCHECK_EXE}")
    else()
      message(SEND_ERROR "cppcheck not found!")
    endif()
  endif()

  # Only pass the options if the tool was found
  if(CPPCHECK_EXE)
    set(CMAKE_C_CPPCHECK
        ${CPPCHECK_EXE} ${ARGN}
        CACHE STRING "" FORCE)
    set(CMAKE_CXX_CPPCHECK
        ${CPPCHECK_EXE} ${ARGN}
        CACHE STRING "" FORCE)
  endif()
endmacro()

# Clears include-what-you-use so it is not called on any following defined code
# compilation. It can be re-enabled by another call to `cppcheck()`.
macro(reset_cppcheck)
  set(CMAKE_C_CPPCHECK
      ""
      CACHE STRING "" FORCE)
  set(CMAKE_CXX_CPPCHECK
      ""
      CACHE STRING "" FORCE)
endmacro()
