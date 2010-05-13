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
ENDFOREACH (example ${examples})

IF (H5_HAVE_PARALLEL)
  ADD_EXECUTABLE (ph5example ${HDF5_EXAMPLES_SOURCE_DIR}/ph5example.c)
  H5_NAMING (ph5example)
  TARGET_LINK_LIBRARIES (ph5example ${HDF5_LIB_TARGET})
ENDIF (H5_HAVE_PARALLEL)
