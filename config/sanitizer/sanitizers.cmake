#
# Copyright (C) 2018 by George Cave - gcave@stablecoder.ca
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

set(USE_SANITIZER
    ""
    CACHE
      STRING
      "Compile with a sanitizer. Options are: Address, Memory, MemoryWithOrigins, Undefined, Thread, Leak, 'Address;Undefined'"
)

function(append value)
  foreach(variable ${ARGN})
    set(${variable}
        "${${variable}} ${value}"
        PARENT_SCOPE)
  endforeach(variable)
endfunction()

message(STATUS "USE_SANITIZER=${USE_SANITIZER}, CMAKE_C_COMPILER_ID=${CMAKE_C_COMPILER_ID}")
if(USE_SANITIZER)
  if(CMAKE_C_COMPILER_ID MATCHES "IntelLLVM" OR CMAKE_C_COMPILER_ID MATCHES "[Cc]lang")
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

    if(UNIX)
      append("-fno-omit-frame-pointer" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
      message(STATUS "Building with sanitize, base flags=${CMAKE_C_SANITIZER_FLAGS}")

      if(uppercase_CMAKE_BUILD_TYPE STREQUAL "DEBUG")
        append("-O1" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
      endif()

      if(USE_SANITIZER MATCHES "([Aa]ddress);([Uu]ndefined)"
         OR USE_SANITIZER MATCHES "([Uu]ndefined);([Aa]ddress)")
        message(STATUS "Building with Address, Undefined sanitizers")
        append("-fsanitize=address,undefined" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        set(MEMCHECK_TYPE AddressSanitizer)
      elseif(USE_SANITIZER MATCHES "([Aa]ddress)")
        # Optional: -fno-optimize-sibling-calls -fsanitize-address-use-after-scope
        message(STATUS "Building with Address sanitizer")
        append("-fsanitize=address" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        set(MEMCHECK_TYPE AddressSanitizer)
      elseif(USE_SANITIZER MATCHES "([Mm]emory([Ww]ith[Oo]rigins)?)")
        # Optional: -fno-optimize-sibling-calls -fsanitize-memory-track-origins=2
        append("-fsanitize=memory" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        if(USE_SANITIZER MATCHES "([Mm]emory[Ww]ith[Oo]rigins)")
          message(STATUS "Building with MemoryWithOrigins sanitizer")
          append("-fsanitize-memory-track-origins" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        else()
          message(STATUS "Building with Memory sanitizer")
        endif()
        set(MEMCHECK_TYPE MemorySanitizer)
      elseif(USE_SANITIZER MATCHES "([Uu]ndefined)")
        message(STATUS "Building with Undefined sanitizer")
        append("-fsanitize=undefined" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        if(EXISTS "${BLACKLIST_FILE}")
          append("-fsanitize-blacklist=${BLACKLIST_FILE}" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        endif()
        set(MEMCHECK_TYPE UndefinedBehaviorSanitizer)
      elseif(USE_SANITIZER MATCHES "([Tt]hread)")
        message(STATUS "Building with Thread sanitizer")
        append("-fsanitize=thread" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
        set(MEMCHECK_TYPE ThreadSanitizer)
      elseif(USE_SANITIZER MATCHES "([Ll]eak)")
        message(STATUS "Building with Leak sanitizer")
        append("-fsanitize=leak" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
       set(MEMCHECK_TYPE LeakSanitizer)
      else()
        message(
          FATAL_ERROR "Unsupported value of USE_SANITIZER: ${USE_SANITIZER}")
      endif()
    elseif(MSVC)
      if(USE_SANITIZER MATCHES "([Aa]ddress)")
        message(STATUS "Building with Address sanitizer")
        append("-fsanitize=address" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
      else()
        message(FATAL_ERROR "This sanitizer not yet supported in the MSVC environment: ${USE_SANITIZER}")
      endif()
    else()
      message(FATAL_ERROR "USE_SANITIZER is not supported on this platform.")
    endif()
  elseif(MSVC)
    if(USE_SANITIZER MATCHES "([Aa]ddress)")
      message(STATUS "Building with Address sanitizer")
      append("/fsanitize=address" CMAKE_C_SANITIZER_FLAGS CMAKE_CXX_SANITIZER_FLAGS)
    else()
      message(FATAL_ERROR "This sanitizer not yet supported in the MSVC environment: ${USE_SANITIZER}")
    endif()
  else()
    message(FATAL_ERROR "USE_SANITIZER is not supported on this platform.")
  endif()
endif()
