cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_HL_TOOLS_H5WATCH)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (H5WATCH_SOURCES
    ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/h5watch
)

#-- Add h5watch program
include_directories (${HDF5_TOOLS_DIR}/lib)
include_directories (${HDF5_HL_TOOLS_DIR}/src)
include_directories (${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR})

add_executable (h5watch ${H5WATCH_SOURCES})
TARGET_NAMING (h5watch STATIC)
TARGET_C_PROPERTIES (h5watch STATIC " " " ")
target_link_libraries (h5watch ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h5watch PROPERTIES FOLDER tools/hl)

if (BUILD_TESTING)
  include_directories (${HDF5_TEST_SRC_DIR})
  include_directories(${HDF5_HL_SRC_DIR}/test)
  #-- Add swmr_check_compat_vfd program
  set (hl_swmr_check_compat_vfd_SOURCES
      ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/swmr_check_compat_vfd.c
  )
  add_executable (hl_swmr_check_compat_vfd ${hl_swmr_check_compat_vfd_SOURCES})
  TARGET_NAMING (hl_swmr_check_compat_vfd STATIC)
  TARGET_C_PROPERTIES (hl_swmr_check_compat_vfd STATIC " " " ")
  target_link_libraries (hl_swmr_check_compat_vfd ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (hl_swmr_check_compat_vfd PROPERTIES FOLDER tools/hl)

  #-- Add extend_dset program
  set (extend_dset_SOURCES
      ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/extend_dset.c
  )
  add_executable (extend_dset ${extend_dset_SOURCES})
  TARGET_NAMING (extend_dset STATIC)
  TARGET_C_PROPERTIES (extend_dset STATIC " " " ")
  target_link_libraries (extend_dset ${HDF5_HL_LIB_TARGET} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  set_target_properties (extend_dset PROPERTIES FOLDER tools/hl)

  add_executable (h5watchgentest ${HDF5_HL_TOOLS_H5WATCH_SOURCE_DIR}/h5watchgentest.c)
  TARGET_NAMING (h5watchgentest STATIC)
  TARGET_C_PROPERTIES (h5watchgentest STATIC " " " ")
  target_link_libraries (h5watchgentest ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
  set_target_properties (h5watchgentest PROPERTIES FOLDER generator/tools/hl)

  include (CMakeTests.cmake)
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    TARGETS
        h5watch
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hltoolsapplications
)
