HDF5-DIAG: Error detected in HDF5 (version (number)) thread (IDs):
  #000: (file name) line (number) in H5Dcreate2(): unable to create dataset
    major: Dataset
    minor: Unable to initialize object
  #001: (file name) line (number) in H5D__create_named(): unable to create and link to dataset
    major: Dataset
    minor: Unable to initialize object
  #002: (file name) line (number) in H5L_link_object(): unable to create new link to object
    major: Links
    minor: Unable to initialize object
  #003: (file name) line (number) in H5L_create_real(): can't insert link
    major: Symbol table
    minor: Unable to insert object
  #004: (file name) line (number) in H5G_traverse(): internal path traversal failed
    major: Symbol table
    minor: Object not found
  #005: (file name) line (number) in H5G_traverse_real(): traversal operator failed
    major: Symbol table
    minor: Callback failed
  #006: (file name) line (number) in H5L_link_cb(): unable to create object
    major: Object header
    minor: Unable to initialize object
  #007: (file name) line (number) in H5O_obj_create(): unable to open object
    major: Object header
    minor: Can't open object
  #008: (file name) line (number) in H5O__dset_create(): unable to create dataset
    major: Dataset
    minor: Unable to initialize object
  #009: (file name) line (number) in H5D__create(): unable to construct layout information
    major: Dataset
    minor: Unable to initialize object
  #010: (file name) line (number) in H5D__chunk_construct(): dimensionality of chunks doesn't match the dataspace
    major: Dataset
    minor: Bad value
H5tools-DIAG: Error detected in HDF5:tools (version (number)) thread (IDs):
  #000: (file name) line (number) in do_copy_objects(): H5Dcreate2 failed
    major: Failure in tools library
    minor: error in function
