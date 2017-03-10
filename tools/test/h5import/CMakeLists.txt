cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  # --------------------------------------------------------------------
  # Add the h5import executables
  # --------------------------------------------------------------------
  add_executable (h5importtest ${HDF5_TOOLS_TEST_H5IMPORT_SOURCE_DIR}/h5importtest.c)
  TARGET_NAMING (h5importtest STATIC)
  TARGET_C_PROPERTIES (h5importtest STATIC " " " ")
  target_link_libraries (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5importtest PROPERTIES FOLDER tools)

  include (CMakeTests.cmake)
