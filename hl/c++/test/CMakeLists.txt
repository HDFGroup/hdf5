cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_CPP_TEST CXX)

add_executable (hl_ptableTest ${HDF5_HL_CPP_TEST_SOURCE_DIR}/ptableTest.cpp)
set_property(TARGET hl_ptableTest APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_HL_SRC_DIR}/test;${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (hl_ptableTest STATIC " " " ")
target_link_libraries (
    hl_ptableTest
    ${HDF5_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
    ${HDF5_CPP_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_HL_CPP_LIB_TARGET}
)
set_target_properties (hl_ptableTest PROPERTIES FOLDER test/hl/cpp)

include (CMakeTests.cmake)
