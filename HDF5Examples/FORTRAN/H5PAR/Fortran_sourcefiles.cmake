#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples
  ph5_f90_dataset
  ph5_f90_file_create
  ph5_f90_hyperslab_by_row
  ph5_f90_hyperslab_by_col
  ph5_f90_hyperslab_by_pattern
  ph5_f90_hyperslab_by_chunk
)

if (HDF5_ENABLE_SUBFILING_VFD)
  set (examples ${examples}
    ph5_f90_subfiling
  )
endif()
if (HDF5_VERSION_STRING VERSION_GREATER_EQUAL "1.14.4")
  set (examples ${examples}
    ph5_f90_filtered_writes_no_sel
  )
endif()
