cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_HL_CPP_EXAMPLES)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_HL_CPP_SRC_DIR}/src)

# --------------------------------------------------------------------
#  Add in the examples for the Packet Table codes
# --------------------------------------------------------------------
IF (HDF5_BUILD_EXAMPLES)
  ADD_EXECUTABLE (ptExampleFL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleFL.cpp)
  H5_NAMING (ptExampleFL)
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (ptExampleFL
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (
      ptExampleFL
      ${HDF5_HL_CPP_LIB_TARGET}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )

  ADD_EXECUTABLE (ptExampleVL ${HDF5_HL_CPP_EXAMPLES_SOURCE_DIR}/ptExampleVL.cpp)
  H5_NAMING (ptExampleVL)
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (ptExampleVL
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (
      ptExampleVL
      ${HDF5_HL_CPP_LIB_TARGET}
      ${HDF5_HL_LIB_TARGET}
      ${HDF5_LIB_TARGET}
  )
ENDIF (HDF5_BUILD_EXAMPLES)
