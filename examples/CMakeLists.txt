cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_EXAMPLES)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
SET (examples
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
)

FOREACH (example ${examples})
  ADD_EXECUTABLE (${example} ${HDF5_EXAMPLES_SOURCE_DIR}/${example}.c)
  H5_NAMING (${example})
  TARGET_LINK_LIBRARIES (${example} ${HDF5_LIB_TARGET})

  IF (BUILD_TESTING)
    ADD_TEST (NAME ${example} COMMAND $<TARGET_FILE:${example}>)
  ENDIF (BUILD_TESTING)
ENDFOREACH (example ${examples})

IF (BUILD_TESTING)
  FILE (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)
ENDIF (BUILD_TESTING)

IF (H5_HAVE_PARALLEL)
  ADD_EXECUTABLE (ph5example ${HDF5_EXAMPLES_SOURCE_DIR}/ph5example.c)
  H5_NAMING (ph5example)
  TARGET_LINK_LIBRARIES (ph5example ${HDF5_LIB_TARGET})

  IF (BUILD_TESTING)
    ADD_TEST (NAME ph5example COMMAND $<TARGET_FILE:ph5example>)
  ENDIF (BUILD_TESTING)
ENDIF (H5_HAVE_PARALLEL)
