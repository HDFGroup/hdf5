cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_CPP_TEST CXX)

add_executable (hl_ptableTest ${HDF5_HL_CPP_TEST_SOURCE_DIR}/ptableTest.cpp)
target_include_directories (hl_ptableTest PRIVATE "${HDF5_HL_SRC_DIR}/test;${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (hl_ptableTest STATIC)
  target_link_libraries (hl_ptableTest PRIVATE
      ${HDF5_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_CPP_LIB_TARGET}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_HL_CPP_LIB_TARGET}
  )
else ()
  TARGET_C_PROPERTIES (hl_ptableTest SHARED)
  target_link_libraries (hl_ptableTest PRIVATE
      ${HDF5_LIBSH_TARGET}
      ${HDF5_TEST_LIBSH_TARGET}
      ${HDF5_CPP_LIBSH_TARGET}
      ${HDF5_HL_LIBSH_TARGET}
      ${HDF5_HL_CPP_LIBSH_TARGET}
  )
endif ()
set_target_properties (hl_ptableTest PROPERTIES FOLDER test/hl/cpp)

include (CMakeTests.cmake)
