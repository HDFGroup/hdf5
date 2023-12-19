#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples)

set (common_examples
    h5ex_g_create
    h5ex_g_iterate
    h5ex_g_traverse
  )

if (HDF5_VERSION_MAJOR VERSION_GREATER_EQUAL "1.8" AND NOT ${EXAMPLE_VARNAME}_USE_16_API)
  set (1_8_examples
      h5ex_g_compact
      h5ex_g_corder
      h5ex_g_phase
      h5ex_g_intermediate
      h5ex_g_visit
  )
else ()
  set (1_8_examples)
endif ()
