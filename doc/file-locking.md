# File Locking in HDF5

This document describes the file locking scheme that was added to HDF5 in
version 1.10.0 and how you can work around it, if you choose to do so. I'll
try to keep it understandable for everyone. We're in the process of converting
the HDF5 user guide (UG) to Doxygen and this document will eventually be
rolled up into those files as we update things.

## Why?

The short answer is: "To prevent you from corrupting your HDF files and/or
crashing your HDF5 reader processes."

The long answer is more complicated.

An HDF5 file's state exists in two places when it is being written to:

1. The HDF5 file itself
2. The HDF5 library's various caches

One of those caches is the metadata cache, which stores things like B-tree
nodes that we use to locate data in the file. Problems arise when parent
objects are flushed to storage before child objects. If a reader tries to
load unflushed children, it will encounter library failures as it tries
to access the non-existant objects.

Keep in mind that the HDF5 library is not analogous to a database server. The
HDF5 library is just a simple shared library, like libjpeg. Library state is
maintained per-library-instance and there is no IPC between HDF5 libraries
loaded by different processes (exception: collective operations in parallel
HDF5, but that's not what were talking about here).

Prior to HDF5 1.10.0, concurrent access to an HDF5 file by multiple processes,
when one or more processes is a writer, was not supported. There was no
enforcement mechanism for this. We simply told people not to do it.

In HDF5 1.10.0, we updated the library to allow the single-writer / multiple-readers
(SWMR - pronounced "swimmer") access pattern. This setup allows one writer and
multiple readers to access the same file, as long as a certain protocol was
followed concerning file opening order and setting the right flags.


## The existing scheme



## Disabling the locks



## Feature Matrix

Include:

- Versions (8, 10, 12, 14)
- Locks exist? Which platforms?
- Different ways to disable



## Appendix: Developer considerations

I


### Locking: POSIX-y systems (Linux, MacOS, etc.)



### Locking: Windows


