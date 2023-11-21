#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples
  ph5_filtered_writes
  ph5_filtered_writes_no_sel
  ph5_dataset
  ph5_file_create
  ph5_hyperslab_by_row
  ph5_hyperslab_by_col
  ph5_hyperslab_by_pattern
  ph5_hyperslab_by_chunk
)
if (${HDF5_ENABLE_SUBFILING_VFD})
    list (APPEND examples ph5_subfiling)
endif ()
