cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_H5DUMP)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  # --------------------------------------------------------------------
  # Add the h5dump test executable
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5dumpgentest ${HDF5_TOOLS_TEST_H5DUMP_SOURCE_DIR}/h5dumpgentest.c)
    TARGET_NAMING (h5dumpgentest STATIC)
    TARGET_C_PROPERTIES (h5dumpgentest STATIC " " " ")
    target_link_libraries (h5dumpgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    set_target_properties (h5dumpgentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5dumpgentest COMMAND $<TARGET_FILE:h5dumpgentest>)
  endif ()

  include (CMakeTests.cmake)

  include (CMakeTestsPBITS.cmake)

  include (CMakeTestsVDS.cmake)

  include (CMakeTestsXML.cmake)
