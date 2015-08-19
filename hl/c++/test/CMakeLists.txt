cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_HL_CPP_TEST)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/c++/src)

# --------------------------------------------------------------------
# Add in the unit tests for the packet table c++ wrapper
# --------------------------------------------------------------------

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/test)
INCLUDE_DIRECTORIES (${HDF5_CPP_SRC_DIR}/src)

add_executable (hl_ptableTest ${HDF5_HL_CPP_TEST_SOURCE_DIR}/ptableTest.cpp)
TARGET_C_PROPERTIES (hl_ptableTest STATIC " " " ")
TARGET_NAMING (hl_ptableTest STATIC)
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
