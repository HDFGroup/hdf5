cmake_minimum_required (VERSION 3.10)
project (HDF5_TOOLS_TEST_H5JAM C)

# --------------------------------------------------------------------
# Add the h5jam test executables
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS)
  add_executable (h5jamgentest ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/h5jamgentest.c)
  set_property(TARGET h5jamgentest APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
  TARGET_C_PROPERTIES (testhdf5 STATIC " " " ")
  target_link_libraries (h5jamgentest ${HDF5_LIB_TARGET})
  set_target_properties (h5jamgentest PROPERTIES FOLDER generator/tools)

  #add_test (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)
endif ()

add_executable (getub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/getub.c)
set_property(TARGET getub APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (getub STATIC " " " ")
target_link_libraries (getub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (getub PROPERTIES FOLDER tools)

add_executable (tellub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/tellub.c)
set_property(TARGET tellub APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_TOOLS_DIR}/lib;${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (tellub STATIC " " " ")
target_link_libraries (tellub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (tellub PROPERTIES FOLDER tools)

include (CMakeTests.cmake)
