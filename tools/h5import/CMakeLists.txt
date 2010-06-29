cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5IMPORT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5import and test executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5import ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5import.c)
H5_NAMING (h5import)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (h5import
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (h5import  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5import)

IF (BUILD_TESTING)
  ADD_EXECUTABLE (h5importtest ${HDF5_TOOLS_H5IMPORT_SOURCE_DIR}/h5importtest.c)
  H5_NAMING (h5importtest)
  IF (WIN32)
    IF (MSVC)
      IF (NOT BUILD_SHARED_LIBS)
        SET_TARGET_PROPERTIES (h5importtest
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (NOT BUILD_SHARED_LIBS)
    ENDIF (MSVC)
  ENDIF (WIN32)
  TARGET_LINK_LIBRARIES (h5importtest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

  ADD_TEST (NAME h5importtest COMMAND $<TARGET_FILE:h5importtest>)

  SET (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
        h5importtest
  )
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5import
    RUNTIME DESTINATION
        bin/tools
    COMPONENT
        toolsapplications
)
