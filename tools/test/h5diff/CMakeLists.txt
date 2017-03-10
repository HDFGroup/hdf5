cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_TEST_H5DIFF)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  # --------------------------------------------------------------------
  # Add the h5diff and test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5diffgentest ${HDF5_TOOLS_TEST_H5DIFF_SOURCE_DIR}/h5diffgentest.c)
    TARGET_NAMING (h5diffgentest STATIC)
    TARGET_C_PROPERTIES (h5diffgentest STATIC " " " ")
    target_link_libraries (h5diffgentest ${HDF5_LIB_TARGET})
    set_target_properties (h5diffgentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5diffgentest COMMAND $<TARGET_FILE:h5diffgentest>)
  endif (HDF5_BUILD_GENERATORS)

  include (CMakeTests.cmake)
