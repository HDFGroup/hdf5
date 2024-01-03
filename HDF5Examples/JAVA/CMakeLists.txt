cmake_minimum_required (VERSION 3.18)
project (HDFJAVA_EXAMPLES Java)

set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES 
    "${HDFJAVA_LIB_DIR};${JAVA_INCLUDE_PATH};${JAVA_INCLUDE_PATH2}"
)

add_subdirectory (H5D)
add_subdirectory (H5T)
add_subdirectory (H5G)
add_subdirectory (H5J)
