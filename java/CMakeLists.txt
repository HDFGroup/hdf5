cmake_minimum_required (VERSION 3.10)
project (HDF5_JAVA C Java)

set (CMAKE_MODULE_PATH "${HDF_RESOURCES_DIR};${HDF_RESOURCES_EXT_DIR}")
find_package (Java)

#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
include (${HDF_RESOURCES_DIR}/UseJava.cmake)

message (STATUS "JAVA: JAVA_HOME=$ENV{JAVA_HOME} JAVA_ROOT=$ENV{JAVA_ROOT}")
find_package (JNI)
message (STATUS "JNI_LIBRARIES=${JNI_LIBRARIES}")
message (STATUS "JNI_INCLUDE_DIRS=${JNI_INCLUDE_DIRS}")


if (WIN32)
  set (HDF_JRE_DIRECTORY "C:/Program Files/Java/jre")
else ()
  set (HDF_JRE_DIRECTORY "/usr/lib/jvm/jre")
endif ()

#-----------------------------------------------------------------------------
# Include the main src and config directories
#-----------------------------------------------------------------------------
set (HDF5_JAVA_INCLUDE_DIRECTORIES
    ${JNI_INCLUDE_DIRS}
    ${HDF5_JAVA_JNI_SRC_DIR}
    ${JAVA_INCLUDE_PATH}
    ${JAVA_INCLUDE_PATH2}
)
set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES "${HDF5_JAVA_INCLUDE_DIRECTORIES}")

set (CMAKE_JAVA_INCLUDE_PATH "")

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (src)

#-----------------------------------------------------------------------------
# Build the Java Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (examples)
endif ()

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (test)
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
