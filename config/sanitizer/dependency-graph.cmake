#
# Copyright (C) 2019 by George Cave - gcave@stablecoder.ca
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

find_program(DOT_EXE "dot")
mark_as_advanced(FORCE DOT_EXE)
if(DOT_EXE)
  message(STATUS "dot found: ${DOT_EXE}")
else()
  message(STATUS "dot not found!")
endif()

if(NOT DOT_EXE)
  option(
    BUILD_DEP_GRAPH
    "Builds a visual representation of the dependencies of that included targets"
    OFF)
else()
  option(
    BUILD_DEP_GRAPH
    "Builds a visual representation of the dependencies of that included targets"
    ON)
endif()

# Builds a dependency graph of the active code targets using the `dot`
# application
#
# This can only be used once per project, as each target generated is as
# `doc-${PROJECT_NAME}` unless TARGET_NAME is specified.
# ~~~
# Required Arguments:
# OUTPUT_TYPE
#   This is the output type, which doubles as the output file type, such as pdf, png.
#   This can be whatever the `dot` application allows.
#
# Options Arguments:
# ADD_TO_DEP_GRAPH
#   If specified, add this generated target to be a dependency of the more general
#   `dep-graph` target.
#
# TARGET_NAME <str>
#   The name to give the doc target. (Default: dep-graph-${PROJECT_NAME})
#
# OUTPUT_DIR <str>
#   The directory to place the generated output
# ~~~
function(gen_dep_graph OUTPUT_TYPE)
  set(OPTIONS ADD_TO_DEP_GRAPH)
  set(SINGLE_VALUE_KEYWORDS TARGET_NAME OUTPUT_DIR)
  set(MULTI_VALUE_KEYWORDS)
  cmake_parse_arguments(gen_dep_graph "${OPTIONS}" "${SINGLE_VALUE_KEYWORDS}"
                        "${MULTI_VALUE_KEYWORDS}" ${ARGN})

  if(BUILD_DEP_GRAPH)
    if(NOT DOT_EXE)
      message(FATAL_ERROR "`dot` is needed to build the dependency graph.")
    endif()

    if(gen_dep_graph_TARGET_NAME)
      set(TARGET_NAME ${gen_dep_graph_TARGET_NAME})
    else()
      set(TARGET_NAME dep-graph-${PROJECT_NAME})
    endif()

    if(gen_dep_graph_OUTPUT_DIR)
      set(OUT_DIR ${gen_dep_graph_OUTPUT_DIR})
    else()
      set(OUT_DIR ${CMAKE_CURRENT_BINARY_DIR})
    endif()

    add_custom_target(
      ${TARGET_NAME}
      COMMAND ${CMAKE_COMMAND} ${CMAKE_SOURCE_DIR}
              --graphviz=${CMAKE_CURRENT_BINARY_DIR}/graphviz/${TARGET_NAME}.dot
      COMMAND
        ${DOT_EXE} -T${OUTPUT_TYPE}
        ${CMAKE_CURRENT_BINARY_DIR}/graphviz/${TARGET_NAME}.dot -o
        ${OUT_DIR}/${TARGET_NAME}.${OUTPUT_TYPE})

    add_custom_command(
      TARGET ${TARGET_NAME}
      POST_BUILD
      COMMAND ;
      COMMENT
        "Dependency graph for ${TARGET_NAME} generated and located at ${OUT_DIR}/${TARGET_NAME}.${OUTPUT_TYPE}"
    )

    if(gen_dep_graph_ADD_TO_DEP_GRAPH)
      if(NOT TARGET dep-graph)
        add_custom_target(dep-graph)
      endif()

      add_dependencies(dep-graph ${TARGET_NAME})
    endif()
  endif()
endfunction()
