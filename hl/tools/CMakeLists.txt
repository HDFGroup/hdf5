cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_TOOLS)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (GIF2H5_SRCS
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif.h
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/decompress.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif2hdf.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gif2mem.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/gifread.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/writehdf.c
)

#-- Add gif2hdf5 program
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5)

ADD_EXECUTABLE (gif2h5 ${GIF2H5_SRCS})
H5_NAMING (gif2h5)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (gif2h5
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (gif2h5 ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

#-- Add h52gif program
SET (hdf2gif_SRCS
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/hdf2gif.c
    ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/hdfgifwr.c
)
ADD_EXECUTABLE (hdf2gif ${hdf2gif_SRCS})
H5_NAMING (hdf2gif)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (hdf2gif
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (hdf2gif ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

# --------------------------------------------------------------------
# This executable can generate the actual test files - Currently not
#  used in the CMake Build system as we rely on the test files that are
#  shipped with HDF5 source archives
# --------------------------------------------------------------------
IF (NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (hl_h52gifgentest ${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5/h52gifgentst.c)
    H5_NAMING (hl_h52gifgentest)
    IF (WIN32)
      IF (MSVC)
        SET_TARGET_PROPERTIES (hl_h52gifgentest
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (MSVC)
    ENDIF (WIN32)
    TARGET_LINK_LIBRARIES (hl_h52gifgentest ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
    
#    ADD_TEST (NAME hl_h52gifgentest COMMAND $<TARGET_FILE:hl_h52gifgentest>)
ENDIF (NOT BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        gif2h5
        hdf2gif
    RUNTIME DESTINATION
        bin/tools
    COMPONENT
        hltoolsapplications
)
