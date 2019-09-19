cmake_minimum_required (VERSION 3.10)
project (HDFJAVA_EXAMPLES_INTRO Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set (HDF_JAVA_EXAMPLES
    H5_CreateAttribute
    H5_CreateDataset
    H5_CreateFile
    H5_CreateGroup
    H5_CreateGroupAbsoluteRelative
    H5_CreateGroupDataset
    H5_ReadWrite
)

if (WIN32 OR MINGW)
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ";")
else ()
  set (CMAKE_JAVA_INCLUDE_FLAG_SEP ":")
endif ()

set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVA_JARS}")

set (CMAKE_JAVA_CLASSPATH ".")
foreach (CMAKE_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${CMAKE_INCLUDE_PATH}")
endforeach ()

foreach (example ${HDF_JAVA_EXAMPLES})
  file (WRITE ${PROJECT_BINARY_DIR}/${example}_Manifest.txt
  "Main-Class: examples.intro.${example}
"
  )
  add_jar (${example} MANIFEST ${PROJECT_BINARY_DIR}/${example}_Manifest.txt ${example}.java)
  get_target_property (${example}_JAR_FILE ${example} JAR_FILE)
#  install_jar (${example} ${HJAVA_INSTALL_DATA_DIR}/examples examples)
  get_target_property (${example}_CLASSPATH ${example} CLASSDIR)
  add_dependencies (${example} ${HDF5_JAVA_HDF5_LIB_TARGET})
endforeach ()

set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVA_JARS};${HDF5_JAVA_LOGGING_JAR};${HDF5_JAVA_LOGGING_NOP_JAR}")

set (CMAKE_JAVA_CLASSPATH ".")
foreach (HDFJAVA_JAR ${CMAKE_JAVA_INCLUDE_PATH})
  set (CMAKE_JAVA_CLASSPATH "${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${HDFJAVA_JAR}")
endforeach ()

if (BUILD_TESTING)
  get_property (target_name TARGET ${HDF5_JAVA_JNI_LIB_TARGET} PROPERTY OUTPUT_NAME)
  set (CMD_ARGS "-Dhdf.hdf5lib.H5.loadLibraryName=${target_name}$<$<CONFIG:Debug>:${CMAKE_DEBUG_POSTFIX}>;")

  foreach (example ${HDF_JAVA_EXAMPLES})
    add_test (
        NAME JAVA_intro-${example}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ${HDFJAVA_EXAMPLES_BINARY_DIR}/${example}.h5
    )

    add_test (
        NAME JAVA_intro-${example}-copy-objects
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${HDFJAVA_EXAMPLES_SOURCE_DIR}/testfiles/examples.intro.${example}.txt
            ${HDFJAVA_EXAMPLES_INTRO_BINARY_DIR}/${example}.txt
    )
    set_tests_properties (JAVA_intro-${example}-copy-objects PROPERTIES DEPENDS JAVA_intro-${example}-clear-objects)

    add_test (
        NAME JAVA_intro-${example}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_TESTER=${CMAKE_Java_RUNTIME};${CMAKE_Java_RUNTIME_FLAGS}"
            -D "TEST_PROGRAM=examples.intro.${example}"
            -D "TEST_ARGS:STRING=${CMD_ARGS}"
            -D "TEST_CLASSPATH:STRING=${CMAKE_JAVA_CLASSPATH}${CMAKE_JAVA_INCLUDE_FLAG_SEP}${${example}_JAR_FILE}"
            -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
            -D "TEST_FOLDER=${HDFJAVA_EXAMPLES_BINARY_DIR}"
            -D "TEST_OUTPUT=intro/${example}.out"
            -D "TEST_EXPECT=0"
            -D "TEST_REFERENCE=intro/${example}.txt"
            -P "${HDF_RESOURCES_DIR}/jrunTest.cmake"
    )
    set_tests_properties (JAVA_intro-${example} PROPERTIES
        DEPENDS JAVA_intro-${example}-copy-objects
    )
  endforeach ()

endif ()
