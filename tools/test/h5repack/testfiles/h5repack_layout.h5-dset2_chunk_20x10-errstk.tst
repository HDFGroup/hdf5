HDF5-DIAG: Error detected in HDF5 (version (number)) thread (IDs):
  #000: (file name) line (number) in H5Dcreate2(): unable to synchronously create dataset
    major: Dataset
    minor: Unable to create file
  #001: (file name) line (number) in H5D__create_api_common(): unable to create dataset
    major: Dataset
    minor: Unable to create file
  #002: (file name) line (number) in H5VL_dataset_create(): dataset create failed
    major: Virtual Object Layer
    minor: Unable to create file
  #003: (file name) line (number) in H5VL__dataset_create(): dataset create failed
    major: Virtual Object Layer
    minor: Unable to create file
  #004: (file name) line (number) in H5VL__native_dataset_create(): unable to create dataset
    major: Dataset
    minor: Unable to initialize object
  #005: (file name) line (number) in H5D__create_named(): unable to create and link to dataset
    major: Dataset
    minor: Unable to initialize object
  #006: (file name) line (number) in H5L_link_object(): unable to create new link to object
    major: Links
    minor: Unable to initialize object
  #007: (file name) line (number) in H5L__create_real(): can't insert link
    major: Links
    minor: Unable to insert object
  #008: (file name) line (number) in H5G_traverse(): internal path traversal failed
    major: Symbol table
    minor: Object not found
  #009: (file name) line (number) in H5G__traverse_real(): traversal operator failed
    major: Symbol table
    minor: Callback failed
  #010: (file name) line (number) in H5L__link_cb(): unable to create object
    major: Links
    minor: Unable to initialize object
  #011: (file name) line (number) in H5O_obj_create(): unable to open object
    major: Object header
    minor: Can't open object
  #012: (file name) line (number) in H5O__dset_create(): unable to create dataset
    major: Dataset
    minor: Unable to initialize object
  #013: (file name) line (number) in H5D__create(): unable to construct layout information
    major: Dataset
    minor: Unable to initialize object
  #014: (file name) line (number) in H5D__chunk_construct(): dimensionality of chunks doesn't match the dataspace
    major: Dataset
    minor: Bad value
H5tools-DIAG: Error detected in HDF5:tools (version (number)) thread (IDs):
  #000: (file name) line (number) in do_copy_objects(): H5Dcreate2 failed
    major: Failure in tools library
    minor: function info
