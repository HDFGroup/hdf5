cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_HL_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_HL_CPP_SRC_DIR}/src)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
add_executable (ptExampleFL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleFL.cpp)
TARGET_NAMING (ptExampleFL STATIC)
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
endif (BUILD_TESTING)
