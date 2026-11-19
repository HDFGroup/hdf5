cmake_minimum_required (VERSION 3.26)
project (HDF5_UTILS_SUBFILINGVFD C)

# h5fuse -p compiles a small MPI program at run time to find its rank and size,
# so it needs an MPI C compiler. When MPI was found without a compiler wrapper,
# the C compiler itself is assumed to be one (for example Cray's cc).
if (MPI_C_COMPILER)
  set (H5FUSE_MPI_CC "${MPI_C_COMPILER}")
else ()
  set (H5FUSE_MPI_CC "${CMAKE_C_COMPILER}")
endif ()

configure_file (${HDF5_UTILS_SUBFILINGVFD_SOURCE_DIR}/h5fuse.in ${HDF5_UTILS_SUBFILINGVFD_BINARY_DIR}/h5fuse @ONLY)

# Copy h5fuse to testpar directory for subfiling tests
if (HDF5_ENABLE_PARALLEL AND HDF5_TEST_PARALLEL)
  file (COPY ${HDF5_UTILS_SUBFILINGVFD_BINARY_DIR}/h5fuse
        DESTINATION ${HDF5_TEST_PAR_BINARY_DIR}
  )
endif ()

install (
    FILES ${HDF5_UTILS_SUBFILINGVFD_BINARY_DIR}/h5fuse
    DESTINATION ${HDF5_INSTALL_BIN_DIR}
    PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    COMPONENT utilsapplications
)
