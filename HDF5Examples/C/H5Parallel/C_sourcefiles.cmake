#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples
  ph5_filtered_writes.c
  ph5_filtered_writes_no_sel.c
  ph5_dataset.c
  ph5_file_create.c
  ph5_hyperslab_by_row.c
  ph5_hyperslab_by_col.c
  ph5_hyperslab_by_pattern.c
  ph5_hyperslab_by_chunk.c
)
if (${HDF5_ENABLE_SUBFILING_VFD})
    list (APPEND examples ph5_subfiling.c)
endif ()
