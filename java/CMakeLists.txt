cmake_minimum_required (VERSION 3.26)
project (HDF5_JAVA C Java)

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
set (HDF5_JSRC_FILES
    hdf5_h.java
    hdf5_h_1.java
    hdf5_h_2.java
)

if (Java_VERSION_STRING VERSION_GREATER_EQUAL "24.0.0")
  if (HDF5_ENABLE_JNI)
    set (HDF5_JAVA_USE_FFM FALSE)
    message (STATUS "Building HDF5 Java with JNI implementation (explicitly requested via HDF5_ENABLE_JNI)")
  else ()
    set (HDF5_JAVA_USE_FFM TRUE)
    message (STATUS "Building HDF5 Java with FFM implementation (Java ${Java_VERSION_STRING})")
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
  if (HDF5_ENABLE_ROS3_VFD)
    set (jsrc_features "ros3")
  else ()
    set (jsrc_features "plain")
  endif ()
  foreach (h5_jfile ${HDF5_JSRC_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/jsrc/features/${jsrc_features}/${h5_jfile}" "${PROJECT_SOURCE_DIR}/jsrc/org/hdfgroup/javahdf5/${h5_jfile}" "HDF5_JSRC_files")
  endforeach ()
  add_custom_target(HDF5_JSRC_files ALL COMMENT "Copying files needed by HDF5_JSRC_files tests" DEPENDS ${HDF5_JSRC_files_list})
  add_subdirectory (jsrc)
  configure_file (
    ${PROJECT_SOURCE_DIR}/hdf/hdf5lib/HDF5Features.java.in
    ${PROJECT_SOURCE_DIR}/hdf/hdf5lib/HDF5Features.java ESCAPE_QUOTES @ONLY
  )
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
