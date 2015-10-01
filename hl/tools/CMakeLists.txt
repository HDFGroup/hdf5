cmake_minimum_required (VERSION 3.1.0)
PROJECT (HDF5_HL_TOOLS)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (GIF2H5_SRCS
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif.h
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/decompress.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif2hdf.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif2mem.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gifread.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/writehdf.c
)

#-- Add gif2hdf5 program
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5)

add_executable (gif2h5 ${GIF2H5_SRCS})
TARGET_NAMING (gif2h5 STATIC)
TARGET_C_PROPERTIES (gif2h5 STATIC " " " ")
target_link_libraries (gif2h5 ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (gif2h5 PROPERTIES FOLDER tools/hl)

#-- Add h52gif program
set (hdf2gif_SRCS
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/hdf2gif.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/hdfgifwr.c
)
add_executable (h52gif ${hdf2gif_SRCS})
TARGET_NAMING (h52gif STATIC)
TARGET_C_PROPERTIES (h52gif STATIC " " " ")
target_link_libraries (h52gif ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
set_target_properties (h52gif PROPERTIES FOLDER tools/hl)

if (BUILD_TESTING)
  # --------------------------------------------------------------------
  # This executable can generate the actual test files - Currently not
  #  used in the CMake Build system as we rely on the test files that are
  #  shipped with HDF5 source archives
  # --------------------------------------------------------------------
  if (HDF5_BUILD_GENERATORS)
      add_executable (hl_h52gifgentest ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/h52gifgentst.c)
      TARGET_NAMING (hl_h52gifgentest STATIC)
      TARGET_C_PROPERTIES (hl_h52gifgentest STATIC " " " ")
      target_link_libraries (hl_h52gifgentest ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
      set_target_properties (hl_h52gifgentest PROPERTIES FOLDER generator/tools/hl)
    
#      add_test (NAME hl_h52gifgentest COMMAND $<TARGET_FILE:hl_h52gifgentest>)
  endif (HDF5_BUILD_GENERATORS)

  include (CMakeTests.cmake)
endif (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    TARGETS
        gif2h5
        h52gif
    RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hltoolsapplications
)
