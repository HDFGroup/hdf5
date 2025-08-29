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
if (Java_VERSION_STRING VERSION_GREATER_EQUAL "24.0.0" AND NOT HDF5_ENABLE_JNI)
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
