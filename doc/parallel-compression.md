# HDF5 Parallel Compression

## Introduction

When an HDF5 dataset is created, the application can specify
optional data filters to be applied to the dataset (as long as
the dataset uses a chunked data layout). These filters may
perform compression, shuffling, checksumming/error detection
and more on the dataset data. The filters are added to a filter
pipeline for the dataset and are automatically applied to the
data during dataset writes and reads.

Prior to the HDF5 1.10.2 release, a parallel HDF5 application
could read datasets with filters applied to them, but could
not write to those datasets in parallel. The datasets would
have to first be written in a serial HDF5 application or from
a single MPI rank in a parallel HDF5 application. This
restriction was in place because:

 - Updating the data in filtered datasets requires management
   of file metadata, such as the dataset's chunk index and file
   space for data chunks, which must be done collectively in
   order for MPI ranks to have a consistent view of the file.
   At the time, HDF5 lacked the collective coordination of
   this metadata management.

 - When multiple MPI ranks are writing independently to the
   same chunk in a dataset (even if their selected portions of
   the chunk don't overlap), the whole chunk has to be read,
   unfiltered, modified, re-filtered and then written back to
   disk. This read-modify-write style of operation would cause
   conflicts among the MPI ranks and lead to an inconsistent
   view of the file.

Introduced in the HDF5 1.10.2 release, the parallel compression
feature allows an HDF5 application to write in parallel to
datasets with filters applied to them, as long as collective
I/O is used. The feature introduces new internal infrastructure
that coordinates the collective management of the file metadata
between MPI ranks during dataset writes. It also accounts for
multiple MPI ranks writing to a chunk by assigning ownership to
one of the MPI ranks, at which point the other MPI ranks send
their modifications to the owning MPI rank.

The parallel compression feature is always enabled when HDF5
is built with parallel enabled, but the feature may be disabled
if the necessary MPI-3 routines are not available. Therefore,
HDF5 conditionally defines the macro `H5_HAVE_PARALLEL_FILTERED_WRITES`
which an application can check for to see if the feature is
available.

## Examples

Using the parallel compression feature is very similar to using
compression in serial HDF5, except that dataset writes **must**
be collective:

```
hid_t dxpl_id = H5Pcreate(H5P_DATASET_XFER);
H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE);
H5Dwrite(..., dxpl_id, ...);
```

The following are two simple examples of using the parallel
compression feature:

[ph5_filtered_writes.c](https://github.com/HDFGroup/hdf5/blob/hdf5_1_14/HDF5Examples/C/H5PAR/ph5_filtered_writes.c)

[ph5_filtered_writes_no_sel.c](https://github.com/HDFGroup/hdf5/blob/hdf5_1_14/HDF5Examples/C/H5PAR/ph5_filtered_writes_no_sel.c)

The former contains simple examples of using the parallel
compression feature to write to compressed datasets, while the
latter contains an example of how to write to compressed datasets
when one or MPI ranks don't have any data to write to a dataset.
Remember that the feature requires these writes to use collective
I/O, so the MPI ranks which have nothing to contribute must still
participate in the collective write call.

## Multi-dataset I/O support

The parallel compression feature is supported when using the
multi-dataset I/O API routines ([H5Dwrite_multi](https://hdfgroup.github.io/hdf5/group___h5_d.html#gaf6213bf3a876c1741810037ff2bb85d8)/[H5Dread_multi](https://hdfgroup.github.io/hdf5/group___h5_d.html#ga8eb1c838aff79a17de385d0707709915)), but the
following should be kept in mind:

 - Parallel writes to filtered datasets **must** still be collective,
   even when using the multi-dataset I/O API routines

 - When the multi-dataset I/O API routines are passed a mixture of
   filtered and unfiltered datasets, the library currently has to
   perform I/O on them separately in two phases. Since there is
   some slight complexity involved in this, it may be best (depending
   on the number of datasets, number of selected chunks, number of
   filtered vs. unfiltered datasets, etc.) to make two individual
   multi-dataset I/O calls, one for the filtered datasets and one
   for the unfiltered datasets. When performing writes to the datasets,
   this would also allow independent write access to the unfiltered
   datasets if desired, while still performing collective writes to
   the filtered datasets.

## Incremental file space allocation support

HDF5's [file space allocation time](https://hdfgroup.github.io/hdf5/group___d_c_p_l.html#ga85faefca58387bba409b65c470d7d851)
is a dataset creation property that can have significant effects
on application performance, especially if the application uses
parallel HDF5. In a serial HDF5 application, the default file space
allocation time for chunked datasets is "incremental". This means
that allocation of space in the HDF5 file for data chunks is
deferred until data is first written to those chunks. In parallel
HDF5, the file space allocation time was previously always forced
to "early", which allocates space in the file for all of a dataset's
data chunks at creation time (or during the first open of a dataset
if it was created serially). This would ensure that all the necessary
file space was allocated so MPI ranks could perform independent I/O
operations on a dataset without needing further coordination of file
metadata as described previously.

While this strategy has worked in the past, it has some noticeable
drawbacks. For one, the larger the chunked dataset being created,
the more noticeable overhead there will be during dataset creation
as all of the data chunks are being allocated in the HDF5 file.
Further, these data chunks will, by default, be [filled](https://hdfgroup.github.io/hdf5/group___d_c_p_l.html#ga4335bb45b35386daa837b4ff1b9cd4a4)
with HDF5's default fill data value, leading to extraordinary
dataset creation overhead and resulting in pre-filling large
portions of a dataset that the application might have been planning
to overwrite anyway. Even worse, there will be more initial overhead
from compressing that fill data before writing it out, only to have
it read back in, unfiltered and modified the first time a chunk is
written to. In the past, it was typically suggested that parallel
HDF5 applications should use [H5Pset_fill_time](https://hdfgroup.github.io/hdf5/group___d_c_p_l.html#ga6bd822266b31f86551a9a1d79601b6a2)
with a value of `H5D_FILL_TIME_NEVER` in order to disable writing of
the fill value to dataset chunks, but this isn't ideal if the
application actually wishes to make use of fill values. 

With [improvements made](https://www.hdfgroup.org/2022/03/parallel-compression-improvements-in-hdf5-1-13-1/)
to the parallel compression feature for the HDF5 1.13.1 release,
"incremental" file space allocation is now the default for datasets
created in parallel *only if they have filters applied to them*.
"Early" file space allocation is still supported for these datasets
if desired and is still forced for datasets created in parallel that
do *not* have filters applied to them. This change should significantly
reduce the overhead of creating filtered datasets in parallel HDF5
applications and should be helpful to applications that wish to
use a fill value for these datasets. It should also help significantly
reduce the size of the HDF5 file, as file space for the data chunks
is allocated as needed rather than all at once.

## Performance Considerations

Since getting good performance out of HDF5's parallel compression
feature involves several factors, the following is a list of
performance considerations (generally from most to least important)
and best practices to take into account when trying to get the
optimal performance out of the parallel compression feature.

### Begin with a good chunking strategy

[Starting with a good chunking strategy](https://portal.hdfgroup.org/documentation/hdf5-docs/chunking_in_hdf5.html)
will generally have the largest impact on overall application
performance. The different chunking parameters can be difficult
to fine-tune, but it is essential to start with a well-performing
chunking layout before adding compression and parallel I/O into
the mix. Compression itself adds overhead and may have side
effects that necessitate further adjustment of the chunking
parameters and HDF5 application settings. Consider that the
chosen chunk size becomes a very important factor when compression
is involved, as data chunks have to be completely read and
re-written to perform partial writes to the chunk.

[Improving I/O performance with HDF5 compressed datasets](https://docs.hdfgroup.org/archive/support/HDF5/doc/TechNotes/TechNote-HDF5-ImprovingIOPerformanceCompressedDatasets.pdf)
is a useful reference for more information on getting good
performance when using a chunked dataset layout.

### Avoid chunk sharing

Since the parallel compression feature has to assign ownership
of data chunks to a single MPI rank in order to avoid the
previously described read-modify-write issue, an HDF5 application
may need to take care when determining how a dataset will be
divided up among the MPI ranks writing to it. Each dataset data
chunk that is written to by more than 1 MPI rank will incur extra
MPI overhead as one of the ranks takes ownership and the other
ranks send it their data and information about where in the chunk
that data belongs. While not always possible to do, an HDF5
application will get the best performance out of parallel compression
if it can avoid writing in a way that causes more than 1 MPI rank
to write to any given data chunk in a dataset.

### Collective metadata operations

The parallel compression feature typically works with a significant
amount of metadata related to the management of the data chunks
in datasets. In initial performance results gathered from various
HPC machines, it was found that the parallel compression feature
did not scale well at around 8k MPI ranks and beyond. On further
investigation, it became obvious that the bottleneck was due to
heavy filesystem pressure from the metadata management for dataset
data chunks as they changed size (as a result of data compression)
and moved around in the HDF5 file.

Enabling collective metadata operations in the HDF5 application
(as in the below snippet) showed significant improvement in
performance and scalability and is generally always recommended
unless application performance shows negative benefits by doing
so.

```
...
hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);
H5Pset_fapl_mpio(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);
H5Pset_all_coll_metadata_ops(fapl_id, 1);
H5Pset_coll_metadata_write(fapl_id, 1);
hid_t file_id = H5Fcreate("file.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
...
```

### Align chunks in the file

The natural layout of an HDF5 file may cause dataset data
chunks to end up at addresses in the file that do not align
well with the underlying file system, possibly leading to
poor performance. As an example, Lustre performance is generally
good when writes are aligned with the chosen stripe size.
The HDF5 application can use [H5Pset_alignment](https://hdfgroup.github.io/hdf5/group___f_a_p_l.html#gab99d5af749aeb3896fd9e3ceb273677a)
to have a bit more control over where objects in the HDF5
file end up. However, do note that setting the alignment
of objects generally wastes space in the file and has the
potential to dramatically increase its resulting size, so
caution should be used when choosing the alignment parameters.

[H5Pset_alignment](https://hdfgroup.github.io/hdf5/group___f_a_p_l.html#gab99d5af749aeb3896fd9e3ceb273677a)
has two parameters that control the alignment of objects in
the HDF5 file, the "threshold" value and the alignment
value. The threshold value specifies that any object greater
than or equal in size to that value will be aligned in the
file at addresses which are multiples of the chosen alignment
value. While the value 0 can be specified for the threshold
to make every object in the file be aligned according to
the alignment value, this isn't generally recommended, as it
will likely waste an excessive amount of space in the file.

In the example below, the chosen dataset chunk size is
provided for the threshold value and 1MiB is specified for
the alignment value. Assuming that 1MiB is an optimal
alignment value (e.g., assuming that it matches well with
the Lustre stripe size), this should cause dataset data
chunks to be well-aligned and generally give good write
performance.

```
hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);
H5Pset_fapl_mpio(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);
/* Assuming Lustre stripe size is 1MiB, align data chunks
   in the file to address multiples of 1MiB. */
H5Pset_alignment(fapl_id, dataset_chunk_size, 1048576);
hid_t file_id = H5Fcreate("file.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
```

### File free space managers

As data chunks in a dataset get written to and compressed,
they can change in size and be relocated in the HDF5 file.
Since parallel compression usually involves many data chunks
in a file, this can create significant amounts of free space
in the file over its lifetime and eventually cause performance
issues.

An HDF5 application can use [H5Pset_file_space_strategy](https://hdfgroup.github.io/hdf5/group___f_c_p_l.html#ga167ff65f392ca3b7f1933b1cee1b9f70)
with a value of `H5F_FSPACE_STRATEGY_PAGE` to enable the paged
aggregation feature, which can accumulate metadata and raw
data for dataset data chunks into well-aligned, configurably
sized "pages" for better performance. However, note that using
the paged aggregation feature will cause any setting from
[H5Pset_alignment](https://hdfgroup.github.io/hdf5/group___f_a_p_l.html#gab99d5af749aeb3896fd9e3ceb273677a)
to be ignored. While an application should be able to get
comparable performance effects by [setting the size of these pages](https://hdfgroup.github.io/hdf5/group___f_c_p_l.html#gad012d7f3c2f1e1999eb1770aae3a4963) to be equal to the value that
would have been set for [H5Pset_alignment](https://hdfgroup.github.io/hdf5/group___f_a_p_l.html#gab99d5af749aeb3896fd9e3ceb273677a),
this may not necessarily be the case and should be studied.

Note that [H5Pset_file_space_strategy](https://hdfgroup.github.io/hdf5/group___f_c_p_l.html#ga167ff65f392ca3b7f1933b1cee1b9f70)
has a `persist` parameter. This determines whether or not the
file free space manager should include extra metadata in the
HDF5 file about free space sections in the file. If this
parameter is `false`, any free space in the HDF5 file will
become unusable once the HDF5 file is closed. For parallel
compression, it's generally recommended that `persist` be set
to `true`, as this will keep better track of file free space
for data chunks between accesses to the HDF5 file.

```
hid_t fcpl_id = H5Pcreate(H5P_FILE_CREATE);
/* Use persistent free space manager with paged aggregation */
H5Pset_file_space_strategy(fcpl_id, H5F_FSPACE_STRATEGY_PAGE, 1, 1);
/* Assuming Lustre stripe size is 1MiB, set page size to that */
H5Pset_file_space_page_size(fcpl_id, 1048576);
...
hid_t file_id = H5Fcreate("file.h5", H5F_ACC_TRUNC, fcpl_id, fapl_id);
```

### Low-level collective vs. independent I/O

While the parallel compression feature requires that the HDF5
application set and maintain collective I/O at the application
interface level (via [H5Pset_dxpl_mpio](https://hdfgroup.github.io/hdf5/group___d_x_p_l.html#ga001a22b64f60b815abf5de8b4776f09e)),
it does not require that the actual MPI I/O that occurs at
the lowest layers of HDF5 be collective; independent I/O may
perform better depending on the application I/O patterns and
parallel file system performance, among other factors. The
application may use [H5Pset_dxpl_mpio_collective_opt](https://hdfgroup.github.io/hdf5/group___d_x_p_l.html#gacb30d14d1791ec7ff9ee73aa148a51a3)
to control this setting and see which I/O method provides the
best performance.

```
hid_t dxpl_id = H5Pcreate(H5P_DATASET_XFER);
H5Pset_dxpl_mpio(dxpl_id, H5FD_MPIO_COLLECTIVE);
H5Pset_dxpl_mpio_collective_opt(dxpl_id, H5FD_MPIO_INDIVIDUAL_IO); /* Try independent I/O */
H5Dwrite(..., dxpl_id, ...);
```

### Runtime HDF5 Library version

An HDF5 application can use the [H5Pset_libver_bounds](https://hdfgroup.github.io/hdf5/group___f_a_p_l.html#gacbe1724e7f70cd17ed687417a1d2a910)
routine to set the upper and lower bounds on library versions
to use when creating HDF5 objects. For parallel compression
specifically, setting the library version to the latest available
version can allow access to better/more efficient chunk indexing
types and data encoding methods. For example:

```
...
hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);
H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
hid_t file_id = H5Fcreate("file.h5", H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
...
```
