cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_TEST_H5DWALK)

if (HDF5_BUILD_PARALLEL_TOOLS)
  add_custom_command(
    OUTPUT ${HDF5_TOOLS_ROOT_DIR}/test/demo_destfiles.test
    COMMAND bash -c ${HDF5_TOOLS_SRC_H5DWALK_SOURCE_DIR}/copy_demo_files.sh
    ARGS ${HDF5_TOOLS_ROOT_DIR}/test ${CMAKE_BINARY_DIR}/bin
    DEPENDS ${HDF5_TOOLS_SRC_H5DWALK_SOURCE_DIR}/copy_demo_files.sh
  )
endif ()

if (HDF5_TEST_TOOLS AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
