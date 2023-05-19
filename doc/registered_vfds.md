# Registered Virtual File Drivers

Valid VFD identifiers can have values from 0 through 255 for VFDs defined by the HDF5 library. Values 256 through 511 are available for testing new VFDs. Subsequent values should be obtained by contacting the [HDF Help Desk](mailto:help@hdfgroup.org).

Please contact the maintainer of a VFD for help with the plugin.

| Driver | ID  | Search Name | Description | URL | Contact |
| ---    | --- | ---         | ---         | --- | ---     |
| CUDA GPU  | 512 | `gds`  | The HDF5 GPUDirect Storage VFD is a Virtual File Driver (VFD) for HDF5 that can be used to interface with Nvidia's GPUDirect Storage (GDS) API. The driver is built as a plugin library that is external to HDF5.| [Link](https://github.com/hpc-io/vfd-gds)	| [Suren Byna](mailto:sbyna@lbl.gov) |
| GDAL vsil |	513	| `vsil` | The HDF5 GDAL vsil Storage VFD is a Virtual File Driver (VFD) for the GDAL HDF5 driver that can be used to access any file supported by the [GDAL Virtual File System Interface](https://gdal.org/user/virtual_file_systems.html). | [Link](https://github.com/OSGeo/gdal/blob/master/frmts/hdf5/hdf5vfl.h) | [Even Rouault](mailto:even.rouault@spatialys.com) |
| Unidata/UCAR NetCDF-C ByteRange |	514 |	`byte-range` | The Unidata `H5FDhttp.[ch]` VFD driver is used to support accessing remote files using the HTTP byte range mechanism. It is part of the Unidata Netcdf-C library. | [Link](https://github.com/Unidata/netcdf-c/blob/main/libhdf5/H5FDhttp.c)	| [Dennis Heimbigner](mailto:dmh@ucar.edu) |
