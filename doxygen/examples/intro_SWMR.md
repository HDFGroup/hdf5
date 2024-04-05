---
title: Introduction to Single-Writer\_Multiple-Reader (SWMR)
redirect\_from:

---
##\*\*\* UNDER CONSTRUCTION \*\*\*

# Introduction to Single-Writer\_Multiple-Reader (SWMR)

Introduction to SWMR
The Single-Writer / Multiple-Reader (SWMR) feature enables multiple processes to read an HDF5 file while it is being written to (by a single process) without using locks or requiring communication between processes.



All communication between processes must be performed via the HDF5 file. The HDF5 file under SWMR access must reside on a system that complies with POSIX write() semantics.

The basic engineering challenge for this to work was to ensure that the readers of an HDF5 file always see a coherent (though possibly not up to date) HDF5 file.

The issue is that when writing data there is information in the metadata cache in addition to the physical file on disk: 

However, the readers can only see the state contained in the physical file:



The SWMR solution implements dependencies on when the metadata can be flushed to the file. This ensures that metadata cache flush operations occur in the proper order, so that there will never be internal file pointers in the physical file that point to invalid (unflushed) file addresses.

A beneficial side effect of using SWMR access is better fault tolerance. It is more difficult to corrupt a file when using SWMR.


Documentation
[SWMR User's Guide](https://docs.hdfgroup.org/hdf5/tn/HDF5_SWMR_User_Guide.pdf)

HDF5 Library APIs
Page:
H5F\_START\_SWMR\_WRITE — Enables SWMR writing mode for a file
Page:
H5DO\_APPEND — Appends data to a dataset along a specified dimension
Page:
H5P\_SET\_OBJECT\_FLUSH\_CB — Sets a callback function to invoke when an object flush occurs in the file
Page:
H5P\_GET\_OBJECT\_FLUSH\_CB — Retrieves the object flush property values from the file access property list
Page:
H5O\_DISABLE\_MDC\_FLUSHES — Prevents metadata entries for an HDF5 object from being flushed from the metadata cache to storage
Page:
H5O\_ENABLE\_MDC\_FLUSHES — Enables flushing of dirty metadata entries from a file’s metadata cache
Page:
H5O\_ARE\_MDC\_FLUSHES\_DISABLED — Determines if an HDF5 object has had flushes of metadata entries disabled
Tools
Page:
h5watch — Outputs new records appended to a dataset as the dataset grows
Page:
h5format\_convert — Converts the layout format version and chunked indexing types of datasets created with HDF5-1.10 so that applications built with HDF5-1.8 can access them
Page:
h5clear — Clears superblock status\_flags field, removes metadata cache image, prints EOA and EOF, or sets EOA of a file
Design Documents
Error while fetching page properties report data:

Programming Model
Please be aware that the SWMR feature requires that an HDF5 file be created with the latest file format. See H5P\_SET\_LIBVER\_BOUNDS for more information.

To use SWMR follow the the general programming model for creating and accessing HDF5 files and objects along with the steps described below.

SWMR Writer:
The SWMR writer either opens an existing file and objects or creates them as follows.

Open an existing file:

Call H5Fopen using the H5F\_ACC\_SWMR\_WRITE flag.
Begin writing datasets.
Periodically flush data.
Create a new file:

Call H5Fcreate using the latest file format.
Create groups, datasets and attributes, and then close the attributes.
Call H5F\_START\_SWMR\_WRITE to start SWMR access to the file.
Periodically flush data.
Example Code:

Create the file using the latest file format property:

   fapl = H5Pcreate (H5P\_FILE\_ACCESS); 
   status = H5Pset\_libver\_bounds (fapl, H5F\_LIBVER\_LATEST, H5F\_LIBVER\_LATEST); 
   fid = H5Fcreate (filename, H5F\_ACC\_TRUNC, H5P\_DEFAULT, fapl); 
[Create objects (files, datasets, ...). Close any attributes and named datatype objects. Groups and datasets may remain open before starting SWMR access to them.]

Start SWMR access to the file:

   status = H5Fstart\_swmr\_write (fid);  
Reopen the datasets and start writing, periodically flushing data:

   status = H5Dwrite (dset\_id, ...);
   status = H5Dflush (dset\_id); 
SWMR Reader:
The SWMR reader must continually poll for new data:

 

Call H5Fopen using the H5F\_ACC\_SWMR\_READ flag.
Poll, checking the size of the dataset to see if there is new data available for reading.
Read new data, if any.
Example Code:

Open the file using the SWMR read flag:

   fid = H5Fopen (filename, H5F\_ACC\_RDONLY | H5F\_ACC\_SWMR\_READ, H5P\_DEFAULT);
Open the dataset and then repeatedly poll the dataset, by getting the dimensions, reading new data, and refreshing:

   dset\_id = H5Dopen (...);
   space\_id = H5Dget\_space (...);
   while (...) { 
      status = H5Dread (dset\_id, ...);
      status = H5Drefresh (dset\_id);
      space\_id = H5Dget\_space (...);
   }

Limitations and Scope
An HDF5 file under SWMR access must reside on a system that complies with POSIX write() semantics. It is also limited in scope as follows:

The writer process is only allowed to modify raw data of existing datasets by;

Appending data along any unlimited dimension.
Modifying existing data
The following operations are not allowed (and the corresponding HDF5 files will fail):

The writer cannot add new objects to the file.
The writer cannot delete objects in the file.
The writer cannot modify or append data with variable length, string or region reference datatypes.
File space recycling is not allowed. As a result the size of a file modified by a SWMR writer may be larger than a file modified by a non-SWMR writer.

Tools for Working with SWMR
Two new tools, h5watch and h5clear, are available for use with SWMR. The other HDF5 utilities have also been modified to recognize SWMR:

The h5watch tool allows a user to monitor the growth of a dataset.
The h5clear tool clears the status flags in the superblock of an HDF5 file.
The rest of the HDF5 tools will exit gracefully but not work with SWMR otherwise.

Programming Example
A good example of using SWMR is included with the HDF5 tests in the source code. You can run it while reading the file it creates. If you then interrupt the application and reader and look at the resulting file, you will see that the file is still valid. Follow these steps:

Download the HDF5-1.10 source code to a local directory on a filesystem (that complies with POSIX write() semantics). Build the software. No special configuration options are needed to use SWMR.

Invoke two command terminal windows. In one window go into the bin/ directory of the built binaries. In the other window go into the test/ directory of the HDF5-1.10 source code that was just built.

In the window in the test/ directory compile and run use\_append\_chunk.c. The example writes a three dimensional dataset by planes (with chunks of size 1 x 256 x 256).

In the other window (in the bin/ directory) run h5watch on the file created by use\_append\_chunk.c (use\_append\_chunk.h5). It should be run while use\_append\_chunk is executing and you will see valid data displayed with h5watch.

Interrupt use\_append\_chunk while it is running, and stop h5watch.

Use h5clear to clear the status flags in the superbock of the HDF5 file (use\_append\_chunk.h5).

View the file with h5dump. You will see that it is a valid file even though the application did not close properly. It will contain data up to the point that it was interrupted.
