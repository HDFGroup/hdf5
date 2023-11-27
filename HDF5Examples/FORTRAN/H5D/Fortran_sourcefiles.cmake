#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples)

set (common_examples
    h5ex_d_alloc
    h5ex_d_checksum
    h5ex_d_chunk
    h5ex_d_compact
    h5ex_d_extern
    h5ex_d_fillval
    h5ex_d_hyper
    h5ex_d_rdwr
    h5ex_d_unlimmod
    h5ex_d_nbit
#    h5ex_d_sofloat
    h5ex_d_soint
    h5ex_d_transform
)

if (HDF5_ENABLE_Z_LIB_SUPPORT)
  set (common_examples ${common_examples}
      h5ex_d_gzip
  )
endif ()

if (HDF5_ENABLE_SZIP_SUPPORT)
  set (common_examples ${common_examples}
      h5ex_d_szip
  )
endif ()

set (1_10_examples
)
