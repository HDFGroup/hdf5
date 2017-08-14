cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  # --------------------------------------------------------------------
  # Add the h5stat test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5stat_gentest ${HDF5_TOOLS_TEST_H5STAT_SOURCE_DIR}/h5stat_gentest.c)
    TARGET_NAMING (h5stat_gentest STATIC)
    TARGET_C_PROPERTIES (h5stat_gentest STATIC " " " ")
    target_link_libraries (h5stat_gentest ${HDF5_LIB_TARGET})
    set_target_properties (h5stat_gentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5stat_gentest COMMAND $<TARGET_FILE:h5stat_gentest>)
  endif ()

  include (CMakeTests.cmake)
