cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_H5COPY)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

# --------------------------------------------------------------------
# Add the h5copy test executables
# --------------------------------------------------------------------

  if (HDF5_BUILD_GENERATORS)
    add_executable (h5copygentest ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/h5copygentest.c)
    TARGET_NAMING (h5copygentest STATIC)
    TARGET_C_PROPERTIES (h5copygentest STATIC " " " ")
    target_link_libraries (h5copygentest ${HDF5_LIB_TARGET})
    set_target_properties (h5copygentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5copygentest COMMAND $<TARGET_FILE:h5copygentest>)
  endif ()

  include (CMakeTests.cmake)
