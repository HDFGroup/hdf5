cmake_minimum_required (VERSION 3.26)
project (HDF5Examples_JAVA_GROUPS Java)

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
if (Java_VERSION_STRING VERSION_LESS "25.0.0" OR HDF5_PROVIDES_JNI)
  set (CMD_ARGS "-Dhdf.hdf5lib.H5.loadLibraryName=${H5EXAMPLE_JAVA_LIBRARY}$<$<OR:$<CONFIG:Debug>,$<CONFIG:Developer>>:${CMAKE_DEBUG_POSTFIX}>;")
endif ()

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
  add_jar (${EXAMPLE_VARNAME}JC_${example_name}
      SOURCES ${example}
      MANIFEST ${PROJECT_BINARY_DIR}/${example_name}_Manifest.txt
  )
  get_target_property (${EXAMPLE_VARNAME}JC_${example_name}_JAR_FILE ${EXAMPLE_VARNAME}JC_${example_name} JAR_FILE)
  get_target_property (${EXAMPLE_VARNAME}JC_${example_name}_CLASSPATH ${EXAMPLE_VARNAME}JC_${example_name} CLASSDIR)
  if (H5EXAMPLE_JAVA_LIBRARIES)
    add_dependencies (${EXAMPLE_VARNAME}JC_${example_name} ${H5EXAMPLE_JAVA_LIBRARIES})
  endif ()
endforeach ()

if (H5EXAMPLE_BUILD_TESTING)
  macro (ADD_H5_TEST resultfile resultcode)
    add_test (
        NAME ${EXAMPLE_VARNAME}_jcompat-h5-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_JAVA=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_PROGRAM=${resultfile}"
            -D "TEST_ARGS:STRING=${ARGN};${CMD_ARGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${EXAMPLE_VARNAME}JC_${resultfile}_JAR_FILE}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_LIB_DIRECTORY}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${PROJECT_BINARY_DIR}/${resultfile}.out"
            -D "TEST_REFERENCE=${resultfile}.txt"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_SKIP_COMPARE=TRUE"
            -P "${H5EXAMPLE_RESOURCES_DIR}/runTest.cmake"
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (${EXAMPLE_VARNAME}_jcompat-h5-${resultfile} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "${EXAMPLE_VARNAME}_jcompat-h5-${resultfile}")
  endmacro ()

  foreach (example ${HDF_JAVA_EXAMPLES})
    get_filename_component (example_name ${example} NAME_WE)
    add_test (
        NAME ${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-clearall-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${PROJECT_BINARY_DIR}/${example_name}.h5
            ${PROJECT_BINARY_DIR}/${example_name}.out
            ${PROJECT_BINARY_DIR}/${example_name}.out.err
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    add_test (
        NAME ${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-copy-objects
        COMMAND    ${CMAKE_COMMAND}
            -E copy_if_different
            ${PROJECT_SOURCE_DIR}/tfiles/110/${example_name}.txt
            ${PROJECT_BINARY_DIR}/${example_name}.txt
    )
    set_tests_properties (${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-copy-objects PROPERTIES DEPENDS ${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-clearall-objects)
    set (last_test "${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-copy-objects")
    ADD_H5_TEST (${example_name} 0)
    add_test (
        NAME ${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-clean-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            ${PROJECT_BINARY_DIR}/${example_name}.h5
            ${PROJECT_BINARY_DIR}/${example_name}.out
            ${PROJECT_BINARY_DIR}/${example_name}.out.err
    )
    set_tests_properties (${EXAMPLE_VARNAME}_jcompat-h5-${example_name}-clean-objects PROPERTIES DEPENDS ${last_test})
  endforeach ()

endif ()
