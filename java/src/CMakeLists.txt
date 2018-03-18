cmake_minimum_required (VERSION 3.10)
PROJECT ( HDF5_JAVA_SRC C Java )

#-----------------------------------------------------------------------------
# Traverse source subdirectory
#-----------------------------------------------------------------------------
add_subdirectory (jni)
add_subdirectory (hdf)
