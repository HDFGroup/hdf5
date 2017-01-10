cmake_minimum_required(VERSION 3.2.2)
PROJECT ( HDF5_JAVA C Java )

set (CMAKE_MODULE_PATH "${HDF_RESOURCES_DIR};${HDF_RESOURCES_EXT_DIR}")
find_package (Java)
#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
include (${HDF_RESOURCES_DIR}/UseJava.cmake)

message (STATUS "JAVA: JAVA_HOME=$ENV{JAVA_HOME} JAVA_ROOT=$ENV{JAVA_ROOT}")
find_package (JNI)

INCLUDE_DIRECTORIES ( ${JNI_INCLUDE_DIRS} )

if (WIN32)
  set (HDF_JRE_DIRECTORY "C:/Program Files/Java/jre8")
else ()
  set (HDF_JRE_DIRECTORY "/usr/lib/jvm/jre")
endif ()

#-----------------------------------------------------------------------------
# Include the main src and config directories
#-----------------------------------------------------------------------------
set (HDF5_JAVA_INCLUDE_DIRECTORIES
    ${HDF5_JAVA_JNI_SRC_DIR}
    ${JAVA_INCLUDE_PATH}
    ${JAVA_INCLUDE_PATH2}
)
INCLUDE_DIRECTORIES (${HDF5_JAVA_INCLUDE_DIRECTORIES})
set (CMAKE_JAVA_INCLUDE_PATH "")


#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (${HDF5_JAVA_SOURCE_DIR}/src ${HDF5_JAVA_BINARY_DIR}/src)

#-----------------------------------------------------------------------------
# Build the Java Examples
#-----------------------------------------------------------------------------
if (HDF5_BUILD_EXAMPLES)
  add_subdirectory (${HDF5_JAVA_SOURCE_DIR}/examples ${HDF5_JAVA_BINARY_DIR}/examples)
endif ()

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  add_subdirectory (${HDF5_JAVA_SOURCE_DIR}/test ${HDF5_JAVA_BINARY_DIR}/test)
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
