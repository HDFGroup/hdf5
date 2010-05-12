cmake_minimum_required (VERSION 2.8)
# --------------------------------------------------------------------
# Notes: When creating unit test executables they should be prefixed
# with "hl_". This allows for easier filtering of the test suite when
# using ctest. An example would be
#  ctest -R hl_
# which would only run the High Level based unit tests.
# --------------------------------------------------------------------

PROJECT (HDF5_HL_TEST)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_HL_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)

INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})
INCLUDE_DIRECTORIES (${HDF5_TEST_SOURCE_DIR})

# --------------------------------------------------------------------
#  Macro used to add a unit test
# --------------------------------------------------------------------
MACRO (HL_ADD_TEST hl_name files)
  ADD_EXECUTABLE (hl_${hl_name} ${hl_name}.c)
  H5_NAMING (hl_${hl_name})
  TARGET_LINK_LIBRARIES (hl_${hl_name}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
  )

  ADD_TEST (NAME hl_${hl_name} COMMAND $<TARGET_FILE:hl_${hl_name}>)

  #-- Copy the necessary files.
  FOREACH (h5_file ${files})
    SET (dest "${HDF5_HL_TEST_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying HL Test File ${h5_file} to ${dest}")
    ADD_CUSTOM_COMMAND (
        TARGET     hl_${hl_name}
        PRE_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_HL_TEST_SOURCE_DIR}/${h5_file} ${dest}
    )

  ENDFOREACH (h5_file ${HL_REFERENCE_TEST_FILES})
ENDMACRO (HL_ADD_TEST)

HL_ADD_TEST (test_ds "dsdata.txt;dslat.txt;dslon.txt;test_ds_be.h5;test_ds_le.h5")
HL_ADD_TEST (test_image "image8.txt;sepia.pal;earth.pal;image24pixel.txt;image24plane.txt;usa.wri")
HL_ADD_TEST (test_lite "dtype_file.txt")
HL_ADD_TEST (test_packet "")
HL_ADD_TEST (test_table "test_table_be.hdf5;test_table_cray.hdf5;test_table_le.hdf5")

# --------------------------------------------------------------------
# This executable is used to generate test files for the test_ds test.
# It should only be run during development when new test files are needed
# --------------------------------------------------------------------
IF (NOT BUILD_SHARED_LIBS)
  ADD_EXECUTABLE (hl_gen_test_ds gen_test_ds.c)
  H5_NAMING (hl_gen_test_ds)
  TARGET_LINK_LIBRARIES (hl_gen_test_ds
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      ${HDF5_TEST_LIB_TARGET}
  )
ENDIF (NOT BUILD_SHARED_LIBS)
