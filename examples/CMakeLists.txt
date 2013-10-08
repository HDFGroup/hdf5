cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_EXAMPLES)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
    h5_crtdat
    h5_rdwt
    h5_crtatt
    h5_crtgrp
    h5_crtgrpar
    h5_crtgrpd
    h5_cmprss
    h5_extend
    h5_subset
    h5_write
    h5_read
    h5_extend_write
    h5_chunk_read
    h5_compound
    h5_group
    h5_select
    h5_attribute
    h5_mount
    h5_reference
    h5_drivers
    h5_ref2reg
    h5_extlink
    h5_elink_unix2win
    h5_shared_mesg
    h5_external_log_native_vol
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (${example} ${HDF5_EXAMPLES_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (${example} ${LIB_TYPE})
  TARGET_C_PROPERTIES (${example} " " " ")
  TARGET_LINK_LIBRARIES (${example} ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (${example} PROPERTIES FOLDER examples)
ENDFOREACH (example ${examples})

IF (H5_HAVE_PARALLEL)
  ADD_EXECUTABLE (ph5example ${HDF5_EXAMPLES_SOURCE_DIR}/ph5example.c)
  TARGET_NAMING (ph5example ${LIB_TYPE})
  TARGET_C_PROPERTIES (ph5example " " " ")
  TARGET_LINK_LIBRARIES (ph5example ${HDF5_LIB_TARGET})
  SET_TARGET_PROPERTIES (ph5example PROPERTIES FOLDER examples)
ENDIF (H5_HAVE_PARALLEL)

#IF (H5_HAVE_EFF)
#  ADD_EXECUTABLE (h5ff_server ${HDF5_EXAMPLES_SOURCE_DIR}/h5ff_server.c)
#  TARGET_LINK_LIBRARIES (h5ff_server ${HDF5_LIB_TARGET})
#  SET_TARGET_PROPERTIES (h5ff_server PROPERTIES FOLDER examples)
#  ADD_EXECUTABLE (h5ff_client ${HDF5_EXAMPLES_SOURCE_DIR}/h5ff_client.c)
#  TARGET_LINK_LIBRARIES (h5ff_client ${HDF5_LIB_TARGET})
#  SET_TARGET_PROPERTIES (h5ff_client PROPERTIES FOLDER examples)
#  ADD_EXECUTABLE (h5ff_client_do ${HDF5_EXAMPLES_SOURCE_DIR}/h5ff_client_do.c)
#  TARGET_LINK_LIBRARIES (h5ff_client_do ${HDF5_LIB_TARGET})
#  SET_TARGET_PROPERTIES (h5ff_client_do PROPERTIES FOLDER examples)
#  ADD_EXECUTABLE (h5ff_client_map ${HDF5_EXAMPLES_SOURCE_DIR}/h5ff_client_map.c)
#  TARGET_LINK_LIBRARIES (h5ff_client_map ${HDF5_LIB_TARGET})
#  SET_TARGET_PROPERTIES (h5ff_client_map PROPERTIES FOLDER examples)
#  ADD_EXECUTABLE (h5ff_client_old_api ${HDF5_EXAMPLES_SOURCE_DIR}/h5ff_client_old_api.c)
#  TARGET_LINK_LIBRARIES (h5ff_client_old_api ${HDF5_LIB_TARGET})
#  SET_TARGET_PROPERTIES (h5ff_client_old_api PROPERTIES FOLDER examples)
#ENDIF (H5_HAVE_EFF)

IF (BUILD_TESTING)
  INCLUDE (CMakeTests.cmake)
ENDIF (BUILD_TESTING)
