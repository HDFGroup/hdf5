cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_TOOLS_TEST_MISC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  # --------------------------------------------------------------------
  # Add the misc test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5repart_gentest ${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/h5repart_gentest.c)
    TARGET_NAMING (h5repart_gentest STATIC)
    TARGET_C_PROPERTIES (h5repart_gentest STATIC " " " ")
    target_link_libraries (h5repart_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5repart_gentest PROPERTIES FOLDER generator/tools)
    #add_test (NAME h5repart_gentest COMMAND $<TARGET_FILE:h5repart_gentest>)

    add_subdirectory (${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/vds)

  endif (HDF5_BUILD_GENERATORS)

  add_executable (h5repart_test ${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/repart_test.c)
  TARGET_NAMING (h5repart_test STATIC)
  TARGET_C_PROPERTIES (h5repart_test STATIC " " " ")
  target_link_libraries (h5repart_test ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (h5repart_test PROPERTIES FOLDER tools)

  include (CMakeTests.cmake)
