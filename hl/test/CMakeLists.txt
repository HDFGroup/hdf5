cmake_minimum_required (VERSION 2.8.11)
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

INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
MACRO (HL_ADD_EXE hl_name)
  add_executable (hl_${hl_name} ${hl_name}.c)
  TARGET_NAMING (hl_${hl_name} ${LIB_TYPE})
  TARGET_C_PROPERTIES (hl_${hl_name} " " " ")
  target_link_libraries (hl_${hl_name}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
  )
  set_target_properties (hl_${hl_name} PROPERTIES FOLDER test/hl)
ENDMACRO (HL_ADD_EXE)

HL_ADD_EXE (test_ds)
HL_ADD_EXE (test_dset_opt)
HL_ADD_EXE (test_image)
HL_ADD_EXE (test_lite)
HL_ADD_EXE (test_packet)
HL_ADD_EXE (test_table)

# --------------------------------------------------------------------
# This executable is used to generate test files for the test_ds test.
# It should only be run during development when new test files are needed
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
  add_executable (hl_gen_test_ds gen_test_ds.c)
  TARGET_NAMING (hl_gen_test_ds ${LIB_TYPE})
  TARGET_C_PROPERTIES (hl_gen_test_ds " " " ")
  target_link_libraries (hl_gen_test_ds
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
  )
  set_target_properties (hl_gen_test_ds PROPERTIES FOLDER test/hl/gen)
endif (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

include (CMakeTests.cmake)
