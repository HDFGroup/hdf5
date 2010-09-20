cmake_minimum_required (VERSION 2.8)
PROJECT(HDF5_HL_F90_SRC C CXX Fortran)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (
    ${CMAKE_Fortran_MODULE_DIRECTORY} 
    ${HDF5_F90_BINARY_DIR} 
    ${HDF5_F90_SRC_DIR}/src
    ${HDF5_F90_SRC_BINARY_DIR}
    ${HDF5_HL_F90_SRC_SOURCE_DIR}
    ${HDF5_HL_SRC_DIR}/src
)
LINK_DIRECTORIES (
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    ${HDF5_HL_F90_BINARY_DIR}
    ${HDF5_F90_BINARY_DIR}
    ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
)
#-----------------------------------------------------------------------------
# List Source files
#-----------------------------------------------------------------------------
SET (HDF5_HL_F90_F_SRCS
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTff.f90
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMff.f90
)

SET (HDF5_HL_F90_C_SRCS
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMfc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5IMcc.c
    ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5TBfc.c
)

SET (HDF5_HL_F90_HEADERS ${HDF5_HL_F90_SRC_SOURCE_DIR}/H5LTf90proto.h)

ADD_LIBRARY (${HDF5_HL_F90_C_LIB_TARGET} ${LIB_TYPE} ${HDF5_HL_F90_C_SRCS} ${HDF5_HL_F90_HEADERS})
TARGET_LINK_LIBRARIES (${HDF5_HL_F90_C_LIB_TARGET}
    ${HDF5_F90_C_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
)
#
# To Ensure that generated files are built, we must force a dependency
ADD_DEPENDENCIES(${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_F90_LIB_TARGET} ${HDF5_F90_C_LIB_TARGET})

SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT
    "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_C_LIB_TARGET}"
)
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_C_LIB_TARGET} ${HDF5_HL_F90_C_LIB_NAME} ${LIB_TYPE})

ADD_LIBRARY (${HDF5_HL_F90_LIB_TARGET} ${LIB_TYPE} ${HDF5_HL_F90_F_SRCS})
IF (BUILD_SHARED_LIBS)
  IF (WIN32)
    SET_PROPERTY (TARGET ${HDF5_HL_F90_LIB_TARGET} 
	    APPEND PROPERTY COMPILE_DEFINITIONS 
		    BUILD_HDF5_DLL
	)
    IF (MSVC)
      SET_TARGET_PROPERTIES (${HDF5_HL_F90_LIB_TARGET}
          PROPERTIES
              COMPILE_FLAGS "/dll"
              LINK_FLAGS "/SUBSYSTEM:CONSOLE /DLL"
      ) 
    ENDIF (MSVC)
  ENDIF (WIN32)
ENDIF (BUILD_SHARED_LIBS)
IF (WIN32)
  SET_PROPERTY (TARGET ${HDF5_HL_F90_LIB_TARGET} 
      APPEND PROPERTY COMPILE_DEFINITIONS 
  	    HDF5F90_WINDOWS
  )
ENDIF (WIN32)
SET_TARGET_PROPERTIES (${HDF5_HL_F90_LIB_TARGET} PROPERTIES LINKER_LANGUAGE Fortran)
TARGET_LINK_LIBRARIES (${HDF5_HL_F90_LIB_TARGET}
    ${HDF5_HL_F90_C_LIB_TARGET}
    ${HDF5_F90_LIB_TARGET}
)
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_F90_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_LIB_NAME} ${LIB_TYPE})
ADD_DEPENDENCIES (${HDF5_HL_F90_LIB_TARGET} ${HDF5_HL_F90_C_LIB_TARGET})

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------

INSTALL (
    DIRECTORY
        ${CMAKE_Fortran_MODULE_DIRECTORY}/\${BUILD_TYPE}/
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}/fortran
    COMPONENT
        fortheaders
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
IF (HDF5_EXPORTED_TARGETS)
  INSTALL (
      TARGETS
          ${HDF5_HL_F90_C_LIB_TARGET}
          ${HDF5_HL_F90_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlfortlibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlfortlibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hlfortlibraries
  )
ENDIF (HDF5_EXPORTED_TARGETS)
