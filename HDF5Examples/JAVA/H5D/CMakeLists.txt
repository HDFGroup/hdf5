cmake_minimum_required (VERSION 3.18)
project (HDF5Examples_JAVA_H5D Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Java_sourcefiles.cmake)

if (WIN32)
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ";")
else ()
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ":")
endif ()

set (CMAKE_JAVA_INCLUDE_PATH ".")
foreach (CMAKE_JINCLUDE_PATH ${HDF5_JAVA_INCLUDE_DIRS})
  set (CMAKE_JAVA_INCLUDE_PATH "${CMAKE_JAVA_INCLUDE_PATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_JINCLUDE_PATH}")
endforeach ()
set (CMD_ARGS "-Dhdf.hdf5lib.H5.loadLibraryName=${H5EX_JAVA_LIBRARY}$<$<OR:$<CONFIG:Debug>,$<CONFIG:Developer>>:${CMAKE_DEBUG_POSTFIX}>;")

set (CMAKE_JAVA_CLASSPATH ".")
foreach (CMAKE_INCLUDE_PATH ${HDF5_JAVA_INCLUDE_DIRS})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
endforeach ()

foreach (HCP_JAR ${HDF5_JAVA_INCLUDE_DIRS})
  get_filename_component (_HCP_FILE ${HCP_JAR} NAME)
  set (HDFJAVA_CLASSJARS "${_HCP_FILE} ${HDFJAVA_CLASSJARS}")
endforeach ()

foreach (example ${HDF_JAVA_EXAMPLES})
  get_filename_component (example_name ${example} NAME_WE)
  file (WRITE ${PROJECT_BINARY_DIR}/${example_name}_Manifest.txt
  "Main-Class: ${example_name}
Class-Path: ${HDFJAVA_CLASSJARS}
"
  )
  add_jar (${EXAMPLE_VARNAME}J_${example_name}
      SOURCES ${example}
      MANIFEST ${PROJECT_BINARY_DIR}/${example_name}_Manifest.txt
  )
  get_target_property (${EXAMPLE_VARNAME}J_${example_name}_JAR_FILE ${EXAMPLE_VARNAME}J_${example_name} JAR_FILE)
  get_target_property (${EXAMPLE_VARNAME}J_${example_name}_CLASSPATH ${EXAMPLE_VARNAME}J_${example_name} CLASSDIR)
  add_dependencies (${EXAMPLE_VARNAME}J_${example_name} ${H5EX_JAVA_LIBRARIES})
endforeach ()

if (H5EX_BUILD_TESTING)
  macro (ADD_H5_TEST resultfile resultcode)
    add_test (
        NAME ${EXAMPLE_VARNAME}_jnative-h5-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_PROGRAM=${resultfile}"
            -D "TEST_ARGS:STRING=${ARGN};${CMD_ARGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${EXAMPLE_VARNAME}J_${resultfile}_JAR_FILE}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${PROJECT_BINARY_DIR}/${resultfile}.out"
            -D "TEST_REFERENCE=${resultfile}.txt"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_SKIP_COMPARE=TRUE"
            -P "${H5EX_RESOURCES_DIR}/jrunTest.cmake"
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
            ${PROJECT_BINARY_DIR}/${example_name}.h5
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
            ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}.txt
            ${PROJECT_BINARY_DIR}/${example_name}.txt
    )
    set_tests_properties (${EXAMPLE_VARNAME}_jnative-h5-${example_name}-copy-objects PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_jnative-h5-${example_name}-clearall-objects)
    set (last_test "${EXAMPLE_VARNAME}_jnative-h5-${example_name}-copy-objects")
    ADD_H5_TEST (${example_name} 0)
  endforeach ()

endif ()
