cmake_minimum_required (VERSION 3.12)
project (HDF5_TOOLS_TEST_DH5WALK)

add_custom_command(
  OUTPUT ${HDF5_TOOLS_DIR}/test/demo_destfiles.test
  COMMAND bash -c ${HDF5_TOOLS_SRC_DH5WALK_SOURCE_DIR}/copy_demo_files.sh
  ARGS ${HDF5_TOOLS_DIR}/test ${CMAKE_BINARY_DIR}/bin
  DEPENDS ${HDF5_TOOLS_SRC_DH5WALK_SOURCE_DIR}/copy_demo_files.sh
)

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
