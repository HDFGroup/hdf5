cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5IMPORT C)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
add_executable (h5importtest ${HDF5_TOOLS_TEST_H5IMPORT_SOURCE_DIR}/h5importtest.c)
target_include_directories (h5importtest PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT ONLY_SHARED_LIBS)
  TARGET_C_PROPERTIES (h5importtest STATIC)
  target_link_libraries (h5importtest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5importtest SHARED)
  target_link_libraries (h5importtest PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5importtest PROPERTIES FOLDER tools)

include (CMakeTests.cmake)
