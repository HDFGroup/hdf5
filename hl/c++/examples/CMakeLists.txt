cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_HL_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_HL_CPP_SRC_DIR}/src)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
ADD_EXECUTABLE (ptExampleFL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleFL.cpp)
TARGET_NAMING (ptExampleFL ${LIB_TYPE})
TARGET_C_PROPERTIES (ptExampleFL " " " ")
TARGET_LINK_LIBRARIES (
    ptExampleFL
    ${HDF5_HL_CPP_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
SET_TARGET_PROPERTIES (ptExampleFL PROPERTIES FOLDER examples/hl/cpp)

IF (BUILD_TESTING)
  INCLUDE (CMakeTests.cmake)
ENDIF (BUILD_TESTING)
