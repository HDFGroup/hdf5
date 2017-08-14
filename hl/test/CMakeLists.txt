cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_HL_TEST)
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

include_directories (${CMAKE_CURRENT_BINARY_DIR})
include_directories (${HDF5_TEST_SRC_DIR})

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
MACRO (HL_ADD_EXE hl_name)
  add_executable (hl_${hl_name} ${hl_name}.c)
  TARGET_NAMING (hl_${hl_name} STATIC)
  TARGET_C_PROPERTIES (hl_${hl_name} STATIC " " " ")
  target_link_libraries (hl_${hl_name}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (hl_${hl_name} PROPERTIES FOLDER test/hl)
ENDMACRO ()

MACRO (HL_ADD_SHEXE hl_name)
  add_executable (hl_${hl_name} ${hl_name}.c)
  TARGET_NAMING (hl_${hl_name} SHARED)
  TARGET_C_PROPERTIES (hl_${hl_name} SHARED " " " ")
  target_link_libraries (hl_${hl_name}
      ${HDF5_HL_LIBSH_TARGET}
      ${HDF5_TEST_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  set_target_properties (hl_${hl_name} PROPERTIES FOLDER test/hl)
ENDMACRO ()

HL_ADD_EXE (test_lite)
HL_ADD_EXE (test_image)
HL_ADD_EXE (test_file_image)
HL_ADD_EXE (test_table)
HL_ADD_EXE (test_ds)
HL_ADD_EXE (test_dset_opt)
HL_ADD_EXE (test_ld)
HL_ADD_EXE (test_dset_append)

# test_packet has two source files
add_executable (hl_test_packet test_packet.c test_packet_vlen.c)
TARGET_NAMING (hl_test_packet STATIC)
TARGET_C_PROPERTIES (hl_test_packet STATIC " " " ")
target_link_libraries (hl_test_packet
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_TEST_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
set_target_properties (hl_test_packet PROPERTIES FOLDER test/hl)

# --------------------------------------------------------------------
# This executable is used to generate test files for the test_ds test.
# It should only be run during development when new test files are needed
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS)
  add_executable (hl_gen_test_ds gen_test_ds.c)
  TARGET_NAMING (hl_gen_test_ds STATIC)
  TARGET_C_PROPERTIES (hl_gen_test_ds STATIC " " " ")
  target_link_libraries (hl_gen_test_ds
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (hl_gen_test_ds PROPERTIES FOLDER test/hl/gen)

  add_executable (hl_gen_test_ld gen_test_ld.c)
  TARGET_NAMING (hl_gen_test_ld STATIC)
  TARGET_C_PROPERTIES (hl_gen_test_ld STATIC " " " ")
  target_link_libraries (hl_gen_test_ld
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
  set_target_properties (hl_gen_test_ld PROPERTIES FOLDER test/hl/gen)
endif ()

include (CMakeTests.cmake)
