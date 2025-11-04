cmake_minimum_required (VERSION 3.26)
project (HDF5_JAVA_SRCJNI C)

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
message (VERBOSE "JAVA: JAVA_HOME=$ENV{JAVA_HOME} JAVA_ROOT=$ENV{JAVA_ROOT}")
find_package (JNI)
message (VERBOSE "JNI_LIBRARIES=${JNI_LIBRARIES}")
message (VERBOSE "JNI_INCLUDE_DIRS=${JNI_INCLUDE_DIRS}")

list (APPEND HDF5_JAVA_INCLUDE_DIRECTORIES
    ${JNI_INCLUDE_DIRS}
    ${HDF5_JAVA_SRCJNI_JNI_SRC_DIR}
)

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (jni)
add_subdirectory (hdf)

#-----------------------------------------------------------------------------
# Testing
#-----------------------------------------------------------------------------
if (NOT HDF5_EXTERNALLY_CONFIGURED AND BUILD_TESTING)
  add_subdirectory (test)
endif ()
