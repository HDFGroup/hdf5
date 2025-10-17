cmake_minimum_required (VERSION 3.26)
project (HDFJAVA_EXAMPLES Java)

set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES 
    "${HDFJAVA_LIB_DIR};${JAVA_INCLUDE_PATH};${JAVA_INCLUDE_PATH2}"
)

if (NOT HDF5_PROVIDES_JNI AND ${H5_LIBVER_DIR} GREATER 114)
  add_subdirectory (H5D)
  add_subdirectory (H5T)
  add_subdirectory (H5G)
  add_subdirectory (TUTR)
endif ()

if (HDF5_PROVIDES_COMPAT OR HDF5_PROVIDES_JNI)
  add_subdirectory (compat)
endif ()
