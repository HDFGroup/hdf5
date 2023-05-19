# Registered Virtual Object Layer Connectors

Valid VOL connector identifiers can have values from 0 through 255 for connectors defined by the HDF5 library. Values 256 through 511 are available for testing new VOL connectors. Subsequent values should be obtained by contacting the the [HDF Help Desk](mailto:help@hdfgroup.org).

Please contact the maintainer of a VOL connector for help with the plugin.

| Connector | ID  | Search Name | Description | URL | Contact |
| ---       | --- | ---         | ---         | --- | ---     |
| native | 0 | `native` | |	| [HDF Help Desk](mailto:help@hdfgroup.org) |
| Asynchronous I/O | 512 |`async` | Provides support for asynchronous operations to HDF5 | [Link](https://github.com/hpc-io/vol-async) | [Suren Byna](mailto:sbyna@lbl.gov) |
| Cache | 513 | `cache` | Provides support for multi-level, multi-location data caching to dataset I/O operations | [Link](https://github.com/hpc-io/vol-cache) | [Suren Byna](mailto:sbyna@lbl.gov) |
| Log-based |	514 | `LOG` | The log-based VOL plugin stores HDF5 datasets in a log-based storage layout. In this layout, data of multiple write requests made by an MPI process are appended one after another in the file. Such I/O strategy can avoid the expensive inter-process communication and I/O serialization due to file lock contentions when storing data in the canonical order. Through the log-based VOL, existing HDF5 programs can achieve a better parallel write performance with minimal changes to their codes. | [Link](https://github.com/DataLib-ECP/vol-log-based/blob/master/README.md) | [Kai Yuan Hou](mailto:khl7265@ece.northwestern.edu) |
| pass-through | 517 | `pass_through_ext` | Provides a simple example of a pass-through VOL connector | [Linkl](https://github.com/hpc-io/vol-external-passthrough) | [Suren Byna](mailto:sbyna@lbl.gov) |
| dset-split | 518 | `dset-split` | Creates separate sub files for each dataset created and mounts these sub-files as external links in the main file. It enables versioning of HDF5 files at a dataset boundary. | [Link](https://github.com/hpc-io/vol-dset-split) | [Annmary Justine](mailto:annmary.roy@hpe.com) |
| PDC-VOL | 519 | `PDC-VOL` |	It is a terminal VOL that reads and writes HDF5 objects to the [PDC system](https://github.com/hpc-io/pdc) | [Link](https://github.com/hpc-io/vol-pdc) | [Houjun Tang](htang4@lbl.gov) |
| REST | 520 | `REST` | Designed to utilize web-based storage systems by use of the HDF5 REST APIs | [Link](https://github.com/HDFGroup/vol-rest)	| [Matthew Larson](mlarson@hdfgroup.org) |
| DAOS | 4004 | `daos` | Designed to utilize the DAOS object storage system by use of the [DAOS API](https://doi.org/10.1109/TPDS.2021.3097884) | [Source](https://github.com/HDFGroup/vol-daos), [Design](https://github.com/HDFGroup/vol-daos/blob/master/docs/design_doc.pdf), [User's Guide](https://github.com/HDFGroup/vol-daos/blob/master/docs/users_guide.pdf) | [HDF Help Desk](mailto:help@hdfgroup.org) |

## Experimental

| Connector | ID  | Search Name | Description | URL | Contact |
| ---       | --- | ---         | ---         | --- | ---     |
| rados |	unassigned |`rados`	| Prototype VOL connector to access data in RADOS | [Link](https://github.com/HDFGroup/vol-rados) | [HDF Help Desk](mailto:help@hdfgroup.org) |
