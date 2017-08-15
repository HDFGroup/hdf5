cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TOOLS_TEST_H5JAM)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib)

  # --------------------------------------------------------------------
  # Add the h5jam test executables
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
    add_executable (h5jamgentest ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/h5jamgentest.c)
    TARGET_NAMING (h5jamgentest STATIC)
    TARGET_C_PROPERTIES (testhdf5 STATIC " " " ")
    target_link_libraries (h5jamgentest ${HDF5_LIB_TARGET})
    set_target_properties (h5jamgentest PROPERTIES FOLDER generator/tools)

    #add_test (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)
  endif ()

add_executable (getub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/getub.c)
TARGET_NAMING (getub STATIC)
TARGET_C_PROPERTIES (getub STATIC " " " ")
target_link_libraries (getub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (getub PROPERTIES FOLDER tools)

add_executable (tellub ${HDF5_TOOLS_TEST_H5JAM_SOURCE_DIR}/tellub.c)
TARGET_NAMING (tellub STATIC)
TARGET_C_PROPERTIES (tellub STATIC " " " ")
target_link_libraries (tellub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
set_target_properties (tellub PROPERTIES FOLDER tools)

  include (CMakeTests.cmake)
