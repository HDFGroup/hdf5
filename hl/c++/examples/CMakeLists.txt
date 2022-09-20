cmake_minimum_required (VERSION 3.18)
project (HDF5_HL_CPP_EXAMPLES CXX)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
add_executable (ptExampleFL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleFL.cpp)
target_include_directories (ptExampleFL PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (ptExampleFL STATIC)
  target_link_libraries (ptExampleFL PRIVATE
      ${HDF5_HL_CPP_LIB_TARGET}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
else ()
  TARGET_C_PROPERTIES (ptExampleFL SHARED)
  target_link_libraries (ptExampleFL PRIVATE
      ${HDF5_HL_CPP_LIBSH_TARGET}
      ${HDF5_HL_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
endif ()
set_target_properties (ptExampleFL PROPERTIES FOLDER examples/hl/cpp)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_HL_CPP_EXAMPLES_FORMAT ptExampleFL)
endif ()

if (BUILD_TESTING AND HDF5_TEST_CPP AND HDF5_TEST_EXAMPLES AND HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
