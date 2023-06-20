cmake_minimum_required (VERSION 3.18)
project (HDF5_DOXYGEN_EXAMPLES C)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
set (examples
    H5A_examples
    H5D_examples
    #H5DO_examples
    H5E_examples
    H5_examples
    H5Fclose
    H5Fcreate
    H5F_examples
    H5G_examples
    H5I_examples
    #H5LDget_dset_elmts
    H5O_examples
    H5P_examples
    #H5Pget_metadata_read_attempts.1
    #H5Pget_metadata_read_attempts.2
    #H5Pget_metadata_read_attempts.3
    #H5Pget_object_flush_cb
    H5PL_examples
    #H5Pset_metadata_read_attempts
    #H5Pset_object_flush_cb
    H5R_examples
    H5S_examples
    #H5TBAget_fill
    H5T_examples
    H5Z_examples
    hello_hdf5
)

foreach (example ${examples})
  add_executable (${example} ${HDF5_DOXYGEN_EXAMPLES_SOURCE_DIR}/${example}.c)
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
    clang_format (HDF5_DOXYGEN_EXAMPLES_${example}_FORMAT ${example})
  endif ()
endforeach ()

if (BUILD_TESTING AND HDF5_TEST_EXAMPLES)
  include (CMakeTests.cmake)
endif ()
