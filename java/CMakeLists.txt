cmake_minimum_required (VERSION 3.26)
project (HDF5_JAVA C Java)

set (CMAKE_MODULE_PATH "${HDF_RESOURCES_DIR}")
find_package (Java)

#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
include (UseJava)

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
# Traverse source subdirectory
#-----------------------------------------------------------------------------
message (VERBOSE "Java version is ${Java_VERSION_STRING}")

# Determine which Java implementation to build based on version and user preferences
set (HDF5_JAVA_USE_FFM FALSE)
set (HDF5_JAVA_IMPLEMENTATION "JNI")

if (Java_VERSION_STRING VERSION_GREATER_EQUAL "24.0.0")
  if (HDF5_ENABLE_JNI)
    set (HDF5_JAVA_USE_FFM FALSE)
    set (HDF5_JAVA_IMPLEMENTATION "JNI")
    message (STATUS "Building HDF5 Java with JNI implementation (explicitly requested via HDF5_ENABLE_JNI)")
  else ()
    set (HDF5_JAVA_USE_FFM TRUE)
    set (HDF5_JAVA_IMPLEMENTATION "FFM")
    message (STATUS "Building HDF5 Java with FFM implementation (Java ${Java_VERSION_STRING})")
  endif ()
else ()
  if (HDF5_ENABLE_JNI)
    message (STATUS "Building HDF5 Java with JNI implementation (Java ${Java_VERSION_STRING})")
  else ()
    message (STATUS "Building HDF5 Java with JNI implementation (Java ${Java_VERSION_STRING} < 24.0.0, FFM not available)")
  endif ()
  if (Java_VERSION_STRING VERSION_LESS "11.0.0")
    message (FATAL_ERROR "Java version ${Java_VERSION_STRING} is not supported. Minimum required: Java 11")
  endif ()
endif ()

# Build appropriate subdirectories based on implementation
if (HDF5_JAVA_USE_FFM)
  add_subdirectory (jsrc)
  add_subdirectory (hdf)
  if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)
    add_subdirectory (jtest)
    add_subdirectory (test)
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
if (HDF5_JAVA_PACK_JRE)
  install (
      DIRECTORY ${HDF_JRE_DIRECTORY}
      DESTINATION ${HDF5_INSTALL_BIN_DIR}
      USE_SOURCE_PERMISSIONS
  )
endif ()
