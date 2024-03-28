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
    h5ex_d_unlimadd
    h5ex_d_unlimmod
)

if (HDF5_ENABLE_Z_LIB_SUPPORT)
  set (common_examples ${common_examples}
      h5ex_d_gzip
      h5ex_d_shuffle
      h5ex_d_unlimgzip
  )
endif ()

if (HDF5_ENABLE_SZIP_SUPPORT)
  set (common_examples ${common_examples}
      h5ex_d_szip
  )
endif ()

if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8" AND NOT ${EXAMPLE_VARNAME}_USE_16_API)
  set (1_8_examples
      h5ex_d_nbit
      h5ex_d_sofloat
      h5ex_d_soint
      h5ex_d_transform
  )
else ()
  set (1_8_examples)
endif ()

