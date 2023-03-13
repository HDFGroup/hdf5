cmake_minimum_required (VERSION 3.18)
project (HDF5_HL_TEST C)
# --------------------------------------------------------------------
# Notes: When creating unit test executables they should be prefixed
# with "hl_". This allows for easier filtering of the test suite when
# using ctest. An example would be
#  ctest -R hl_
# which would only run the High Level based unit tests.
# --------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
set (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
configure_file (${HDF5_HL_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
macro (HL_ADD_EXE hl_name)
  add_executable (hl_${hl_name} ${hl_name}.c)
  target_compile_options(hl_${hl_name} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_include_directories (hl_${hl_name} PRIVATE "${HDF5_TEST_SRC_DIR};${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (hl_${hl_name} STATIC)
    target_link_libraries (hl_${hl_name} PRIVATE
        ${HDF5_HL_LIB_TARGET}
        ${HDF5_TEST_LIB_TARGET}
        ${HDF5_LIB_TARGET}
    )
  else ()
    TARGET_C_PROPERTIES (hl_${hl_name} SHARED)
    target_link_libraries (hl_${hl_name} PRIVATE
        ${HDF5_HL_LIBSH_TARGET}
        ${HDF5_TEST_LIBSH_TARGET}
        ${HDF5_LIBSH_TARGET}
    )
  endif ()
  set_target_properties (hl_${hl_name} PROPERTIES FOLDER test/hl)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_HL_TEST_${hl_name}_FORMAT hl_${hl_name})
  endif ()
endmacro ()

HL_ADD_EXE (test_lite)
HL_ADD_EXE (test_image)
HL_ADD_EXE (test_file_image)
HL_ADD_EXE (test_table)
HL_ADD_EXE (test_ds)
HL_ADD_EXE (test_ld)
HL_ADD_EXE (test_dset_append)
HL_ADD_EXE (test_h5do_compat)

# test_packet has two source files
add_executable (hl_test_packet test_packet.c test_packet_vlen.c)
target_compile_options(hl_test_packet PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_include_directories (hl_test_packet PRIVATE "${HDF5_TEST_SRC_DIR};${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (hl_test_packet STATIC)
  target_link_libraries (hl_test_packet PRIVATE
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
else ()
  TARGET_C_PROPERTIES (hl_test_packet SHARED)
  target_link_libraries (hl_test_packet PRIVATE
      ${HDF5_HL_LIBSH_TARGET}
      ${HDF5_TEST_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
endif ()
set_target_properties (hl_test_packet PROPERTIES FOLDER test/hl)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_HL_TEST_hl_test_packet_FORMAT hl_test_packet)
endif ()

# --------------------------------------------------------------------
# This executable is used to generate test files for the test_ds test.
# It should only be run during development when new test files are needed
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS AND BUILD_STATIC_LIBS)
  add_executable (hl_gen_test_ds gen_test_ds.c)
  target_compile_options(hl_gen_test_ds PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_include_directories (hl_gen_test_ds PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (hl_gen_test_ds STATIC)
  target_link_libraries (hl_gen_test_ds PRIVATE
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (hl_gen_test_ds PROPERTIES FOLDER test/hl/gen)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_HL_TEST_hl_gen_test_ds_FORMAT hl_gen_test_ds)
  endif ()

  add_executable (hl_gen_test_ld gen_test_ld.c)
  target_compile_options(hl_gen_test_ld PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_include_directories (hl_gen_test_ld PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (hl_gen_test_ld STATIC)
  target_link_libraries (hl_gen_test_ld PRIVATE
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (hl_gen_test_ld PROPERTIES FOLDER test/hl/gen)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_HL_TEST_hl_gen_test_ld_FORMAT hl_gen_test_ld)
  endif ()
endif ()

if (HDF5_TEST_SERIAL)
  include (CMakeTests.cmake)
endif ()
