cmake_minimum_required (VERSION 2.8.11)
PROJECT (HDF5_EXAMPLES)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
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
)

foreach (example ${examples})
  add_executable (${example} ${HDF5_EXAMPLES_SOURCE_DIR}/${example}.c)
  TARGET_NAMING (${example} ${LIB_TYPE})
  TARGET_C_PROPERTIES (${example} " " " ")
  target_link_libraries (${example} ${HDF5_LIB_TARGET})
  set_target_properties (${example} PROPERTIES FOLDER examples)
endforeach (example ${examples})

if (H5_HAVE_PARALLEL)
  add_executable (ph5example ${HDF5_EXAMPLES_SOURCE_DIR}/ph5example.c)
  TARGET_NAMING (ph5example ${LIB_TYPE})
  TARGET_C_PROPERTIES (ph5example " " " ")
  target_link_libraries (ph5example ${HDF5_LIB_TARGET})
  set_target_properties (ph5example PROPERTIES FOLDER examples)
endif (H5_HAVE_PARALLEL)

if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif (BUILD_TESTING)
