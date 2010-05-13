cmake_minimum_required (VERSION 2.8)
# --------------------------------------------------------------------
# Notes: When creating unit test executables they should be prefixed
# with "cpp_". This allows for easier filtering of the test suite when
# using ctest. An example would be
#  ctest -R cpp_
# which would only run the C++ based unit tests.
# --------------------------------------------------------------------

PROJECT (HDF5_CPP_TEST)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (CPP_TEST_SRCS
    ${HDF5_CPP_TEST_SOURCE_DIR}/testhdf5.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tattr.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tcompound.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tfile.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tfilter.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/th5s.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tlinks.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/trefer.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/ttypes.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tvlstr.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/dsets.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/h5cpputil.cpp
)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_CPP_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

INCLUDE_DIRECTORIES (${HDF5_TEST_SOURCE_DIR} )

ADD_EXECUTABLE (cpp_testhdf5 ${CPP_TEST_SRCS} )
H5_NAMING (cpp_testhdf5)
TARGET_LINK_LIBRARIES (cpp_testhdf5
    ${HDF5_CPP_LIB_TARGET}
    ${HDF5_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)

ADD_TEST (NAME cpp_testhdf5 COMMAND $<TARGET_FILE:cpp_testhdf5>)
