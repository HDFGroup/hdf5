cmake_minimum_required (VERSION 3.26)
project (HDF5_JAVA C Java)

#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
include (UseJava)
mark_as_advanced (CMAKE_Java_ARCHIVE)
mark_as_advanced (CMAKE_Java_RUNTIME)

if (WIN32)
  set (HDF_JRE_DIRECTORY "C:/Program Files/Java/jre")
else ()
  set (HDF_JRE_DIRECTORY "/usr/lib/jvm/jre")
endif ()

#-----------------------------------------------------------------------------
# Include the main src and config directories
#-----------------------------------------------------------------------------
set (HDF5_JAVA_INCLUDE_DIRECTORIES
    ${JAVA_INCLUDE_PATH}
    ${JAVA_INCLUDE_PATH2}
)
set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES "${HDF5_JAVA_INCLUDE_DIRECTORIES}")

set (CMAKE_JAVA_INCLUDE_PATH "")

#-----------------------------------------------------------------------------
# Configurable paths for Java dependency JARs
#-----------------------------------------------------------------------------
set (HDF5_JAVA_LOGGING_JAR        ${HDF5_JAVA_LIB_DIR}/slf4j-api-2.0.16.jar        CACHE FILEPATH "Path to SLF4J API jar (used at compile and runtime)")
set (HDF5_JAVA_LOGGING_NOP_JAR    ${HDF5_JAVA_LIB_DIR}/ext/slf4j-nop-2.0.16.jar    CACHE FILEPATH "Path to SLF4J NOP binding jar (suppresses logging when no other backend is present)")
set (HDF5_JAVA_LOGGING_SIMPLE_JAR ${HDF5_JAVA_LIB_DIR}/ext/slf4j-simple-2.0.16.jar CACHE FILEPATH "Path to SLF4J simple binding jar (used in tests for basic stdout logging)")
set (HDF5_JAVA_JUNIT_JAR          ${HDF5_JAVA_LIB_DIR}/org.junit.jar   CACHE FILEPATH "Path to JUnit 4 jar (test-only dependency)")
set (HDF5_JAVA_HAMCREST_JAR       ${HDF5_JAVA_LIB_DIR}/org.hamcrest.jar CACHE FILEPATH "Path to Hamcrest jar (test-only dependency, required by JUnit 4)")

#-----------------------------------------------------------------------------
# Validate that all required JAR paths exist
#-----------------------------------------------------------------------------
set (_HDF5_REQUIRED_JAVA_JARS
    HDF5_JAVA_LOGGING_JAR
    HDF5_JAVA_LOGGING_NOP_JAR
    HDF5_JAVA_LOGGING_SIMPLE_JAR
)
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)
  list (APPEND _HDF5_REQUIRED_JAVA_JARS
      HDF5_JAVA_JUNIT_JAR
      HDF5_JAVA_HAMCREST_JAR
  )
endif ()
foreach (_jar_var IN LISTS _HDF5_REQUIRED_JAVA_JARS)
  if (DEFINED ${_jar_var} AND NOT "${${_jar_var}}" STREQUAL "")
    if (NOT EXISTS "${${_jar_var}}")
      message (FATAL_ERROR
          "Configured JAR path '${${_jar_var}}' for cache variable "
          "'${_jar_var}' does not exist. Please set -D${_jar_var}=<valid-path>."
      )
    endif ()
  endif ()
endforeach ()
unset (_HDF5_REQUIRED_JAVA_JARS)

if (Java_VERSION_STRING VERSION_GREATER_EQUAL "25.0.0")
  if (HDF5_ENABLE_JNI)
    set (HDF5_JAVA_USE_FFM FALSE)
    message (STATUS "Building HDF5 Java with JNI implementation (explicitly requested via HDF5_ENABLE_JNI)")
  else ()
    set (HDF5_JAVA_USE_FFM TRUE)
    set (HDF5_ENABLE_JAVA_COMPAT TRUE)
    message (STATUS "Building HDF5 Java with FFM implementation (Java ${Java_VERSION_STRING})")

    # Find the jextract tool using the JEXTRACT_HOME or JAVA_HOME environment variable
    # On Windows, jextract uses .bat extension
    if (WIN32)
        find_program (JEXTRACT_EXECUTABLE
            NAMES jextract.bat jextract
            PATHS "$ENV{JEXTRACT_HOME}/bin" "$ENV{JAVA_HOME}/bin"
            REQUIRED
            NO_DEFAULT_PATH
        )
    else ()
        find_program (JEXTRACT_EXECUTABLE
            NAMES jextract
            PATHS "$ENV{JEXTRACT_HOME}/bin" "$ENV{JAVA_HOME}/bin"
            REQUIRED
            NO_DEFAULT_PATH
        )
    endif ()
    mark_as_advanced (JEXTRACT_EXECUTABLE)

    if (NOT JEXTRACT_EXECUTABLE)
        message (FATAL_ERROR "Could not find jextract executable. "
                             "Please set JEXTRACT_HOME or ensure jextract is in JAVA_HOME/bin\n"
                             "JEXTRACT_HOME=$ENV{JEXTRACT_HOME}\n"
                             "JAVA_HOME=$ENV{JAVA_HOME}")
    endif ()

    # jextract will output to the jsrc binary directory
    # When jsrc/CMakeLists.txt runs, HDF5_JAVA_JSRC_BINARY_DIR will be set to this location by CMake
    set (JEXTRACT_OUTPUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/jsrc)

    # Display jextract configuration
    message (STATUS "jextract executable: ${JEXTRACT_EXECUTABLE}")
    message (STATUS "jextract output directory: ${JEXTRACT_OUTPUT_DIR}")
    message (STATUS "HDF5 source directory: ${HDF5_SRC_DIR}")

    # Test if jextract executable exists and is runnable
    if (NOT EXISTS "${JEXTRACT_EXECUTABLE}")
        message (FATAL_ERROR "jextract executable does not exist at: ${JEXTRACT_EXECUTABLE}")
    endif ()

    # Try to run jextract --version as a test
    message (STATUS "Testing jextract executable...")
    execute_process (
        COMMAND ${JEXTRACT_EXECUTABLE} --version
        RESULT_VARIABLE JEXTRACT_VERSION_RESULT
        OUTPUT_VARIABLE JEXTRACT_VERSION_OUTPUT
        ERROR_VARIABLE JEXTRACT_VERSION_ERROR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_STRIP_TRAILING_WHITESPACE
    )
    message (STATUS "jextract version test result: ${JEXTRACT_VERSION_RESULT}")
    message (STATUS "jextract version output: ${JEXTRACT_VERSION_OUTPUT}")
    if (NOT "${JEXTRACT_VERSION_ERROR}" STREQUAL "")
        message (STATUS "jextract version error: ${JEXTRACT_VERSION_ERROR}")
    endif ()

    # Generate Java bindings with error handling
    message (STATUS "Running jextract to generate FFM bindings...")
    execute_process (
        COMMAND
            ${JEXTRACT_EXECUTABLE}
            --include-dir ${HDF5_SRC_DIR}
            --include-dir ${HDF5_SRC_BINARY_DIR}
            --include-dir ${H5FD_SUBFILING_DIR}
            --output ${JEXTRACT_OUTPUT_DIR}
            --target-package org.hdfgroup.javahdf5
            --library hdf5
            ${HDF5_SRC_DIR}/hdf5.h
        RESULT_VARIABLE JEXTRACT_RESULT
        OUTPUT_VARIABLE JEXTRACT_OUTPUT
        ERROR_VARIABLE JEXTRACT_ERROR
        ECHO_OUTPUT_VARIABLE
        ECHO_ERROR_VARIABLE
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )

    # Check if jextract succeeded
    message (STATUS "jextract exit code: ${JEXTRACT_RESULT}")
    if (NOT JEXTRACT_RESULT EQUAL 0)
        message (STATUS "jextract output length: ${JEXTRACT_OUTPUT}")
        message (STATUS "jextract error length: ${JEXTRACT_ERROR}")
        message (FATAL_ERROR "jextract failed with exit code ${JEXTRACT_RESULT}\n"
                             "Executable: ${JEXTRACT_EXECUTABLE}\n"
                             "Working dir: ${CMAKE_CURRENT_BINARY_DIR}\n"
                             "Output: ${JEXTRACT_OUTPUT}\n"
                             "Error: ${JEXTRACT_ERROR}")
    endif ()

    # Verify that key FFM binding files were generated
    set (EXPECTED_FFM_FILE "${JEXTRACT_OUTPUT_DIR}/org/hdfgroup/javahdf5/hdf5_h.java")
    if (NOT EXISTS "${EXPECTED_FFM_FILE}")
        message (FATAL_ERROR "jextract did not generate expected file: ${EXPECTED_FFM_FILE}")
    endif ()

    message (STATUS "FFM bindings generated successfully at ${JEXTRACT_OUTPUT_DIR}")
  endif ()
else ()
  set (HDF5_JAVA_USE_FFM FALSE)
  set (HDF5_ENABLE_JNI TRUE)
  message (STATUS "Building HDF5 Java with JNI implementation (Java ${Java_VERSION_STRING})")
  if (Java_VERSION_STRING VERSION_LESS "11.0.0")
    message (FATAL_ERROR "Java version ${Java_VERSION_STRING} is not supported. Minimum required: Java 11")
  endif ()
endif ()

# Update global cache variables based on detected implementation
if (HDF5_JAVA_USE_FFM)
  set (HDF5_JAVA_IMPLEMENTATION "FFM" CACHE STRING "Java implementation being built" FORCE)
  set (HDF5_JAVA_ARTIFACT_ID "hdf5-java-ffm" CACHE STRING "Maven artifact ID for Java bindings" FORCE)
  set (DOXYGEN_JAVA_DIR ${HDF5_JAVA_SRC_PATH})
else ()
  set (HDF5_JAVA_IMPLEMENTATION "JNI" CACHE STRING "Java implementation being built" FORCE)
  set (HDF5_JAVA_ARTIFACT_ID "hdf5-java-jni" CACHE STRING "Maven artifact ID for Java bindings" FORCE)
  set (DOXYGEN_JAVA_DIR ${HDF5_JAVA_SRCJNI_PATH})
endif ()
set_property(CACHE HDF5_JAVA_IMPLEMENTATION PROPERTY STRINGS FFM JNI)
mark_as_advanced (HDF5_JAVA_IMPLEMENTATION HDF5_JAVA_ARTIFACT_ID)

# Display final configuration
message (STATUS "Java implementation: ${HDF5_JAVA_IMPLEMENTATION}")
message (STATUS "Java Maven artifact: org.hdfgroup:${HDF5_JAVA_ARTIFACT_ID}")

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
# Build appropriate subdirectories based on implementation
if (HDF5_JAVA_USE_FFM)
  add_subdirectory (jsrc)
  if (HDF5_ENABLE_JAVA_COMPAT)
    add_subdirectory (hdf)
  endif ()
  if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)
    add_subdirectory (jtest)
    if (HDF5_ENABLE_JAVA_COMPAT)
      add_subdirectory (test)
    endif ()
  endif ()
else ()
  add_subdirectory (src-jni)
endif ()

#-----------------------------------------------------------------------------
# Add Required Jar(s)
#-----------------------------------------------------------------------------
install (
    FILES
        ${HDF5_JAVA_LOGGING_JAR}
        ${HDF5_JAVA_LOGGING_NOP_JAR}
        ${HDF5_JAVA_LOGGING_SIMPLE_JAR}
    DESTINATION ${HDF5_INSTALL_JAR_DIR}
    COMPONENT libraries
)

#-----------------------------------------------------------------------------
# Option to include jre
#-----------------------------------------------------------------------------
option (HDF5_JAVA_PACK_JRE  "Package a JRE installer directory" OFF)
mark_as_advanced (HDF5_JAVA_PACK_JRE)
if (HDF5_JAVA_PACK_JRE)
  install (
      DIRECTORY ${HDF_JRE_DIRECTORY}
      DESTINATION ${HDF5_INSTALL_BIN_DIR}
      USE_SOURCE_PERMISSIONS
  )
endif ()
