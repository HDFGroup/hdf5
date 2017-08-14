cmake_minimum_required(VERSION 3.2.2)
PROJECT ( HDF5_JAVA_SRC C Java )

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (${HDF5_JAVA_SRC_SOURCE_DIR}/jni ${HDF5_JAVA_SRC_BINARY_DIR}/jni)
add_subdirectory (${HDF5_JAVA_SRC_SOURCE_DIR}/hdf ${HDF5_JAVA_SRC_BINARY_DIR}/hdf)
