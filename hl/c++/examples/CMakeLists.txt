cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_CPP_EXAMPLES CXX)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
add_executable (ptExampleFL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleFL.cpp)
set_property(TARGET ptExampleFL APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (ptExampleFL STATIC " " " ")
target_link_libraries (
    ptExampleFL
    ${HDF5_HL_CPP_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
set_target_properties (ptExampleFL PROPERTIES FOLDER examples/hl/cpp)

if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif ()
