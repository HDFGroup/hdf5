cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_JAVA_H5D Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

INCLUDE_DIRECTORIES (
    ${HDFJAVA_LIB_DIR}
    ${JAVA_INCLUDE_PATH}
    ${JAVA_INCLUDE_PATH2}
)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Java_sourcefiles.cmake)

if (WIN32)
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ";")
else ()
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ":")
endif ()

set (CMAKE_JAVA_CLASSPATH ".")
foreach (CMAKE_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
endforeach ()

foreach (HCP_JAR ${CMAKE_JAVA_INCLUDE_PATH})
  get_filename_component (_HCP_FILE ${HCP_JAR} NAME)
  set (HDFJAVA_CLASSJARS "${_HCP_FILE} ${HDFJAVA_CLASSJARS}")
endforeach ()

foreach (example ${HDF_JAVA_EXAMPLES})
  get_filename_component (example_name ${example} NAME_WE)
  file (WRITE ${PROJECT_BINARY_DIR}/Manifest.txt
  "Main-Class: ${example_name}
Class-Path: ${HDFJAVA_CLASSJARS}
"
  )
  add_jar (${EXAMPLE_VARNAME}_${example_name} SOURCES ${example} MANIFEST ${PROJECT_BINARY_DIR}/Manifest.txt)
  get_target_property (${EXAMPLE_VARNAME}_${example_name}_JAR_FILE ${EXAMPLE_VARNAME}_${example_name} JAR_FILE)
endforeach ()

if (H5EX_BUILD_TESTING)
  macro (ADD_H5_TEST resultfile resultcode)
    add_test (
        NAME ${EXAMPLE_VARNAME}_jnative-h5-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME}"
            -D "TEST_PROGRAM=${resultfile}"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${EXAMPLE_VARNAME}_${resultfile}_JAR_FILE}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -D "TEST_FOLDER=${HDFJAVA_EXAMPLES_H5D_BINARY_DIR}"
            -D "TEST_OUTPUT=${HDFJAVA_EXAMPLES_H5D_BINARY_DIR}/${resultfile}.out"
            -D "TEST_REFERENCE=${resultfile}.txt"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_SKIP_COMPARE=TRUE"
            -P "${${EXAMPLE_PACKAGE_NAME}_RESOURCES_DIR}/jrunTest.cmake"
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (${EXAMPLE_VARNAME}_jnative-h5-${resultfile} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_jnative-h5-${resultfile}")
  endmacro ()

  foreach (example ${HDF_JAVA_EXAMPLES})
    get_filename_component (example_name ${example} NAME_WE)
    add_test (
        NAME ${EXAMPLE_VARNAME}_jnative-h5-${example_name}-clearall-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${HDFJAVA_EXAMPLES_H5D_BINARY_DIR}/${example_name}.h5
            ${example_name}.out
            ${example_name}.out.err
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (${EXAMPLE_VARNAME}_jnative-h5-${example_name}-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    add_test (
        NAME ${EXAMPLE_VARNAME}_jnative-h5-${example_name}-copy-objects
        COMMAND    ${CMAKE_COMMAND}
            -E copy_if_different
            ${HDFJAVA_EXAMPLES_H5D_SOURCE_DIR}/tfiles/110/${example_name}.txt
            ${HDFJAVA_EXAMPLES_H5D_BINARY_DIR}/${example_name}.txt
    )
    set_tests_properties (${EXAMPLE_VARNAME}_jnative-h5-${example_name}-copy-objects PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_jnative-h5-${example_name}-clearall-objects)
    set (last_test "${EXAMPLE_VARNAME}_jnative-h5-${example_name}-copy-objects")
    ADD_H5_TEST (${example_name} 0)
  endforeach ()

endif ()
