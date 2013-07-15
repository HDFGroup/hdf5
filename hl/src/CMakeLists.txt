cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_HL_SRC)

#-----------------------------------------------------------------------------
# Shared Libs
#-----------------------------------------------------------------------------
IF (BUILD_SHARED_LIBS)
  SET (HL_BUILT_AS_DYNAMIC_LIB 1)
ENDIF (BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)

SET (HL_SRCS
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DO.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DS.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5IM.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LT.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LTanalyze.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5PT.c
    ${HDF5_HL_SRC_SOURCE_DIR}/H5TB.c
)

SET (HL_HEADERS
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DOpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5DSpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5IMpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LTparse.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5LTpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5PTpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/H5TBpublic.h
    ${HDF5_HL_SRC_SOURCE_DIR}/hdf5_hl.h
)

ADD_LIBRARY (${HDF5_HL_LIB_TARGET} ${LIB_TYPE} ${HL_SRCS} ${HL_HEADERS})
TARGET_C_PROPERTIES (${HDF5_HL_LIB_TARGET} " " " ")
TARGET_LINK_LIBRARIES (${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_LIB_TARGET} ${HDF5_HL_LIB_NAME} ${LIB_TYPE})
SET_TARGET_PROPERTIES (${HDF5_HL_LIB_TARGET} PROPERTIES FOLDER libraries/hl)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install 
#-----------------------------------------------------------------------------
INSTALL (
    FILES
        ${HL_HEADERS}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        hlheaders
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
IF (HDF5_EXPORTED_TARGETS)

  INSTALL_TARGET_PDB (${HDF5_HL_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} hllibraries)
  
  INSTALL (
      TARGETS
          ${HDF5_HL_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hllibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hllibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hllibraries
  )
ENDIF (HDF5_EXPORTED_TARGETS)
