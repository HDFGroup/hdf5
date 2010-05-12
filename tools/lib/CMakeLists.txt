cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_LIB)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
#INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
#INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

SET (H5_TOOLS_LIB_SRCS
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5diff.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5diff_array.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5diff_attr.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5diff_dset.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5diff_util.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_filters.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_ref.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_str.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_type.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_utils.c
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5trav.c
)

SET (H5_TOOLS_LIB_HDRS
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5trav.h
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools.h
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_utils.h
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_str.h
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5tools_ref.h
    ${HDF5_TOOLS_SOURCE_DIR}/lib/h5diff.h
)

#-- Always build a static library for linking the ${HDF5_LIB_NAME} tools together
ADD_LIBRARY (${HDF5_TOOLS_LIB_TARGET} ${LIB_TYPE} ${H5_TOOLS_LIB_SRCS} ${H5_TOOLS_LIB_HDRS})
TARGET_LINK_LIBRARIES (${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
SET_GLOBAL_VARIABLE( HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TOOLS_LIB_TARGET}")
H5_SET_LIB_OPTIONS (
    ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TOOLS_LIB_NAME}
    ${LIB_TYPE}
    HDF5_TOOLS_LIB_NAME_RELEASE
	HDF5_TOOLS_LIB_NAME_DEBUG
)

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install 
#-----------------------------------------------------------------------------
INSTALL (
    FILES
        ${H5_TOOLS_LIB_HDRS}
    DESTINATION
        include/tools
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
IF (HDF5_EXPORTED_TARGETS)
  INSTALL (
      TARGETS
          ${HDF5_TOOLS_LIB_TARGET}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION lib/tools
      ARCHIVE DESTINATION lib/tools
      RUNTIME DESTINATION bin/tools
  )
ENDIF (HDF5_EXPORTED_TARGETS)
