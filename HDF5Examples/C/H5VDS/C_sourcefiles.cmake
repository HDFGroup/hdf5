#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples)

if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.10" AND NOT ${EXAMPLE_VARNAME}_USE_16_API AND NOT ${EXAMPLE_VARNAME}_USE_18_API)
  set (1_10_examples
    h5ex_vds
    h5ex_vds-exc
#    h5ex_vds-exclim
    h5ex_vds-eiger
    h5ex_vds-simpleIO
    h5ex_vds-percival
    h5ex_vds-percival-unlim
    h5ex_vds-percival-unlim-maxmin
  )
else ()
  set (1_10_examples)
endif ()

