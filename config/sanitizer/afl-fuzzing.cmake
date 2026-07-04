#
# Copyright (C) 2022 by George Cave - gcave@stablecoder.ca
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

# USAGE: To enable the use of AFL instrumentation, this file needs to be
# included into the CMake scripts at any point *before* any of the compilers are
# setup by CMake, typically at/before the first call to project(), or any part
# before compiler detection/validation occurs.
#
# This is since CMake does not support changing the compiler after it has been
# set.
#
# For example for CMakeLists.txt:
# ~~~
# cmake_minimum_required(VERSION 3.15)
# include(cmake/afl-fuzzing.cmake)
# project(FoE-Engine C CXX)
# ~~~
# And then configuring CMake with: `cmake .. -DAFL_MODE=LTO
# -DAFL_ENV_OPTIONS=AFL_LLVM_THREADSAFE_INST=1;AFL_LLVM_LAF_ALL=1`
#
# Would setup the AFL compiler to use the LTO mode (afl-clang-lto), and prefix
# any build calls to have the two given environment settings, ie:
# `AFL_LLVM_THREADSAFE_INST=1 AFL_LLVM_LAF_ALL=1 afl-clang-lto <...>`
#
# NOTE: If using multiple ENV_OPTIONS, delimit via semi-colons and it will be
# separated correctly.

# Options
option(AFL "Switch to using an AFL compiler" OFF)
set(AFL_MODE
    ""
    CACHE
      STRING
      "Use a specific AFL instrumentation mode: LTO, LLVM, GCC-PLUGIN, CLANG, GCC"
)
set(AFL_ENV_OPTIONS
    ""
    CACHE STRING
          "Add environmental settings to build calls (check `afl-cc -hh`)")

# Sets up for AFL fuzzing by detecting finding and using AFL compilers and
# setting a few flags and environmental build flags as requested.
if(AFL)
  find_program(AFL_C_COMPILER afl-cc)
  find_program(AFL_CXX_COMPILER afl-c++)

  if(AFL_C_COMPILER AND AFL_CXX_COMPILER)
    if((CMAKE_C_COMPILER AND NOT CMAKE_C_COMPILER STREQUAL AFL_C_COMPILER)
       OR (CMAKE_CXX_COMPILER AND NOT CMAKE_CXX_COMPILER STREQUAL
                                  AFL_CXX_COMPILER))
      # CMake doesn't support changing compilers after they've been set
      message(
        FATAL_ERROR
          "Cannot change to AFL compilers after they have been previously set. Clear the cache, reconfigure and ensure setup_afl is called before the first C or CXX compiler is set, typically before the first project() call."
      )
    else()
      # Set the AFL compiler
      message(STATUS "Changed to AFL compiler")
      set(CMAKE_C_COMPILER ${AFL_C_COMPILER})
      set(CMAKE_CXX_COMPILER ${AFL_CXX_COMPILER})

      # Set a specific AFL mode for both compile and link stages
      if(AFL_MODE MATCHES "[Ll][Tt][Oo]")
        message(STATUS "Set AFL to Clang-LTO mode")
        add_compile_options(--afl-lto)
        add_link_options(--afl-lto)
      elseif(AFL_MODE MATCHES "[Ll][Ll][Vv][Mm]")
        message(STATUS "Set AFL to Clang-LLVM mode")
        add_compile_options(--afl-llvm)
        add_link_options(--afl-llvm)
      elseif(AFL_MODE MATCHES "[Gg][Cc][Cc][-_][Pp][Ll][Uu][Gg][Ii][Nn]")
        message(STATUS "Set AFL to GCC-Plugin mode")
        add_compile_options(--afl-gcc-plugin)
        add_link_options(--afl-gcc-plugin)
      elseif(AFL_MODE MATCHES "[Ll][Tt][Oo]")
        message(STATUS "Set AFL to Clang mode")
        add_compile_options(--afl-clang)
        add_link_options(--afl-clang)
      elseif(AFL_MODE MATCHES "[Ll][Tt][Oo]")
        message(STATUS "Set AFL to GCC mode")
        add_compile_options(--afl-gcc)
        add_link_options(--afl-gcc)
      endif()

      # Add specified environment options
      if(AFL_ENV_OPTIONS)
        set(CMAKE_C_COMPILER_LAUNCHER ${CMAKE_C_COMPILER_LAUNCHER}
                                      ${AFL_ENV_OPTIONS})
        set(CMAKE_CXX_COMPILER_LAUNCHER ${CMAKE_CXX_COMPILER_LAUNCHER}
                                        ${AFL_ENV_OPTIONS})
      endif()
    endif()
  else()
    message(FATAL_ERROR "Usable AFL compiler was not found!")
  endif()
endif()
