cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5JAM)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5jam and test executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5jam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jam.c)
H5_NAMING (h5jam)
TARGET_LINK_LIBRARIES (h5jam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_EXECUTABLE (getub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/getub.c)
H5_NAMING (getub)
TARGET_LINK_LIBRARIES (getub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_EXECUTABLE (tellub ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/tellub.c)
H5_NAMING (tellub)
TARGET_LINK_LIBRARIES (tellub  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_EXECUTABLE (h5unjam ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5unjam.c)
H5_NAMING (h5unjam)
TARGET_LINK_LIBRARIES (h5unjam  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES
    h5jam
    getub
    tellub
    h5unjam
)

IF (BUILD_TESTING)
  IF (NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5jamgentest ${HDF5_TOOLS_H5JAM_SOURCE_DIR}/h5jamgentest.c)
    H5_NAMING (h5jamgentest)
    TARGET_LINK_LIBRARIES (h5jamgentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5jamgentest COMMAND $<TARGET_FILE:h5jamgentest>)

    SET (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
    #    h5jamgentest
    )
  ENDIF (NOT BUILD_SHARED_LIBS)
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5jam h5unjam
    RUNTIME DESTINATION
        bin/tools
    COMPONENT
        toolsapplications
)
