$JEXTRACT_HOME/bin/jextract \
  --include-dir $HDF5_HOME/include \
  --output $HDF5_HOME/jsrc \
  --target-package org.hdfgroup.javahdf5 \
  --library hdf5 \
  $HDF5_HOME/include/hdf5.h
