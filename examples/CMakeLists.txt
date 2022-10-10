cmake_minimum_required (VERSION 3.18)
project (HDF5_EXAMPLES C)

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
    h5_ref_extern
    h5_ref_compat
    h5_reference_deprec
    h5_drivers
    h5_ref2reg_deprec
    h5_extlink
    h5_elink_unix2win
    h5_shared_mesg
    h5_debug_trace
    h5_vds
    h5_vds-exc
    h5_vds-exclim
    h5_vds-eiger
    h5_vds-simpleIO
    h5_vds-percival
    h5_vds-percival-unlim
    h5_vds-percival-unlim-maxmin
)

if (H5_HAVE_PARALLEL)
  set (parallel_examples
    ph5example
    ph5_filtered_writes
    ph5_filtered_writes_no_sel
  )

  if (HDF5_ENABLE_SUBFILING_VFD)
    list (APPEND parallel_examples ph5_subfiling)
  endif ()
endif ()

foreach (example ${examples})
  add_executable (${example} ${HDF5_EXAMPLES_SOURCE_DIR}/${example}.c)
  target_include_directories (${example} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${example} STATIC)
    target_link_libraries (${example} PRIVATE ${HDF5_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (${example} SHARED)
    target_link_libraries (${example} PRIVATE ${HDF5_LIBSH_TARGET})
  endif ()
  set_target_properties (${example} PROPERTIES FOLDER examples)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_EXAMPLES_${example}_FORMAT ${example})
  endif ()
endforeach ()

if (H5_HAVE_PARALLEL)
  foreach (parallel_example ${parallel_examples})
    add_executable (${parallel_example} ${HDF5_EXAMPLES_SOURCE_DIR}/${parallel_example}.c)
    target_include_directories (${parallel_example} PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
    if (NOT BUILD_SHARED_LIBS)
      TARGET_C_PROPERTIES (${parallel_example} STATIC)
      target_link_libraries (${parallel_example} PRIVATE ${HDF5_LIB_TARGET} ${MPI_C_LIBRARIES})
    else ()
      TARGET_C_PROPERTIES (${parallel_example} SHARED)
      target_link_libraries (${parallel_example} PRIVATE ${HDF5_LIBSH_TARGET} ${MPI_C_LIBRARIES})
    endif ()
    set_target_properties (${parallel_example} PROPERTIES FOLDER examples)

    #-----------------------------------------------------------------------------
    # Add Target to clang-format
    #-----------------------------------------------------------------------------
    if (HDF5_ENABLE_FORMATTERS)
      clang_format (HDF5_EXAMPLES_${parallel_example}_FORMAT ${parallel_example})
    endif ()
  endforeach ()
endif ()

if (BUILD_TESTING AND HDF5_TEST_EXAMPLES)
  include (CMakeTests.cmake)
endif ()
