#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples)

set (common_examples
    h5ex_g_compact
    h5ex_g_corder
    h5ex_g_phase
    h5ex_g_create
)
if (HDF5_VERSION_STRING VERSION_GREATER_EQUAL "1.10.0")
  set (common_examples
    ${common_examples}
    h5ex_g_intermediate
    h5ex_g_iterate
    h5ex_g_visit
  )
  if (HDF5_VERSION_STRING VERSION_GREATER_EQUAL "1.14.3")
    set (common_examples
      ${common_examples}
      h5ex_g_traverse
    )
  endif()
else ()
  if (HDF_ENABLE_F2003)
    set (common_examples
      ${common_examples}
      h5ex_g_intermediate
      h5ex_g_iterate
      h5ex_g_traverse
      h5ex_g_visit
    )
  endif ()
endif ()
