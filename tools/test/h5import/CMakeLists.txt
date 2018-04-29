cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5IMPORT C)

# --------------------------------------------------------------------
# Add the h5import executables
# --------------------------------------------------------------------
add_executable (h5importtest ${HDF5_TOOLS_TEST_H5IMPORT_SOURCE_DIR}/h5importtest.c)
set_property(TARGET h5importtest APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (h5importtest STATIC " " " ")
target_link_libraries (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5importtest PROPERTIES FOLDER tools)

include (CMakeTests.cmake)
