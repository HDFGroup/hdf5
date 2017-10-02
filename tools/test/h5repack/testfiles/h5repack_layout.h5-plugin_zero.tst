Objects to modify layout areversion (number))     Name
-----------------------------------------
 group                       /
HDF5-DIAG: Error detected in HDF5 (version (number)) thread (IDs):
  #000: (file name) line (number) in H5Pset_filter(): failed to call private function
    major: Property lists
    minor: Can't set value
  #001: (file name) line (number) in H5P__set_filter(): failed to load dynamically loaded plugin
    major: Data filters
    minor: Unable to load metadata into cache
H5tools-DIAG: Error detected in HDF5:tools (version (number)) thread (IDs):
  #000: (file name) line (number) in copy_objects(): do_copy_objects from <h5repack_layout.h5> could not copy data to <out-plugin_zero.h5repack_layout.h5>
    major: Failure in tools library
    minor: error in function
  #001: (file name) line (number) in do_copy_objects(): apply_filters failed
    major: Failure in tools library
    minor: error in function
  #002: (file name) line (number) in apply_filters(): H5Pset_filter failed
    major: Failure in tools library
    minor: error in function
