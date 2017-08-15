cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_CPP_TEST)
# --------------------------------------------------------------------
# Notes: When creating unit test executables they should be prefixed
# with "cpp_". This allows for easier filtering of the test suite when
# using ctest. An example would be
#  ctest -R cpp_
# which would only run the C++ based unit tests.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (CPP_TEST_SOURCES
    ${HDF5_CPP_TEST_SOURCE_DIR}/testhdf5.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tarray.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tattr.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tcompound.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tdspl.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tfile.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tfilter.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/th5s.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/titerate.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tlinks.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tobject.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/trefer.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/ttypes.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/tvlstr.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/dsets.cpp
    ${HDF5_CPP_TEST_SOURCE_DIR}/h5cpputil.cpp
)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
set (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
configure_file (${HDF5_CPP_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)

add_executable (cpp_testhdf5 ${CPP_TEST_SOURCES} )
TARGET_NAMING (cpp_testhdf5 STATIC)
TARGET_C_PROPERTIES (cpp_testhdf5 STATIC " " " ")
target_link_libraries (cpp_testhdf5
    ${HDF5_CPP_LIB_TARGET}
    ${HDF5_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
)
set_target_properties (cpp_testhdf5 PROPERTIES
    FOLDER test/cpp
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)

include (CMakeTests.cmake)
