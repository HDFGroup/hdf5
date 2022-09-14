# File Locking in HDF5

This document describes the file locking scheme that was added to HDF5 in
version 1.10.0 and how you can work around it, if you choose to do so. I'll
try to keep it understandable for everyone, though diving into technical
details is unavoidable, given the complexity of the material. We're in the
process of converting the HDF5 user guide (UG) to Doxygen and this document
will eventually be rolled up into those files as we update things.

**Parallel HDF5 Note**

Everything written here is from the perspective of serial HDF5. When we say
that you can't access a file for write access from more than one process, we
mean "from more than one independent, serial process". Parallel HDF5 can
obviously write to a file from more than one process, but that involves
IPC and multiple processes working together, not independent processes with
no knowledge of each other, which is what the file locks are for.


## Why file locks?

The short answer is: "To prevent you from corrupting your HDF5 files and/or
crashing your reader processes."

The long answer is more complicated.

An HDF5 file's state exists in two places when it is open for writing:

1. The HDF5 file itself
2. The HDF5 library's various caches

One of those caches is the metadata cache, which stores things like B-tree
nodes that we use to locate data in the file. Problems arise when parent
objects are flushed to storage before child objects. If a reader tries to
load unflushed children, the object's file offset could point at garbage
and it will encounter library failures as it tries to access the non-existent
objects.

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
multiple readers to access the same file, as long as a certain protocol is
followed concerning file opening order and setting the right flags. Since
synchronization might be tricky to pull off and the consequences of getting
it wrong could result in corrupt files or crashed readers, we decided to add
a file locking scheme to help users get it right. Since this would also help
prevent harmful accesses when SWMR is not in use, we decided to switch the
file locking scheme on by default. This scheme has been carried forward into
HDF5 1.12 and 1.13 (soon to be 1.14).

Note that the current implementation of SWMR is only useful for appending to chunked
datasets. Creating file objects like groups and datasets is not supported
in the current SWMR implementation.

Unfortunately, this file locking scheme has caused problems for some users.
This is usually people who are working on network file systems like NFS or
on parallel file systems, especially when file locks have been disabled, which
often causes lock calls to fail. As a result of this, we've added work-arounds
to disable the file locking scheme over the years.

## The existing scheme

There are two parts to the file locking scheme. One is the file lock itself.
The second is a mark we make in the HDF5 file's superblock. The superblock
mark isn't really that important for understanding the file locking, but since
it's entwined with the file locking scheme, we'll cover it in the
algorithm below. The lower-level details of file lock implementations are
described in the appendix, but the semantics are straightforward: Locks are
mandatory unless disabled, always for the entire file, and non-blocking. They
are also not required for SWMR operations and simply exist to help you set up
SWMR and prevent dangerous file access.

Here's how it all works:

1. The first thing we do is check if we're using file locks

    - We first check the file locking property in the file access property list
      (fapl). The default value of this property is set at configure time when
      the library is built.
    - Next we check the value of the `HDF5_USE_FILE_LOCKING` environment variable,
      which was previously parsed at library startup. If this is set,
      we use the value to override the property list setting.

    The particulars of the ways you can disable file locks are described in a
    separate section below.

    If we are not using file locking, no further file locking operations will
    take place.

2. We also check for ignoring file locks when they are disabled on the file system.

    - The environment variable setting for this is checked at VFD initialization
      time for all library VFDs.
    - We check the value in the fapl in the `open` callback. The default value for
      this property was set at configure time when the library was built.

3. When we open a file, we lock it based on the file access flags:

    - If the `H5F_ACC_RDWR` flag is set, use an exclusive lock
    - Otherwise use a shared lock

    If we are ignoring disabled file locks (see below), we will silently swallow
    lock API call failure when locks are not implemented on the file system.

4. If the VFD supports locking and the file is open for writing, we mark the
   file consistency flags in the file's superblock to indicate this.

    **NOTE!**

    - The VFD has to have a lock callback for this to happen. It doesn't matter if
      the locking was disabled - the check is simply for the callback.
    - We mark the superblock in **ANY** write case - both SWMR and non-SWMR.
    - Only the latest version of the superblock is marked in this way. If you
      open up a file that wasn't created with the 1.10.0 or later file format,
      it won't get the superblock mark, even if it's been opened for writing.

    According to the file format document and H5Fpkg.h:

    - Bit 0 is set if the file is open for writing (`H5F_SUPER_WRITE_ACCESS`)
    - Bit 2 is set if the file is open for SWMR writing (`H5F_SUPER_SWMR_WRITE_ACCESS`)

    We check these superblock flags on file open and error out if they are
    unsuitable.

    - If the file is already opened for non-SWMR writing, no other process can open
      it.
    - If the file is open for SWMR writing, only SWMR readers can open the file.
    - If you try to open a file for reading with `H5F_ACC_SWMR_READ` set and the
      file does not have the SWMR writer bits set in the superblock, the open
      call will fail.

    This scheme is often confused with the file locking, so it's included here,
    even though it's a bit tangential to the locks themselves.

5. If the file is open for SWMR writing (`H5F_ACC_SWMR_WRITE` is set), we
   remove the file lock just before the open call completes.

6. We normally don't explicitly unlock the file on file close. We let the OS
   handle it when the file descriptors are closed since file locks don't
   normally surivive closing the underlying file descriptor.

**TL;DR**

When locks are available, HDF5 files will be exclusively locked while they are
in use. The exception to this are files that are opened for SWMR writing, which
are unlocked. Files that are open for any kind of writing get a "writing"
superblock mark that HDF5 1.10.0+ will respect and refuse to open outside of SWMR.

## `H5Fstart_swmr_write()`

This API call can be used to switch an open file to "SWMR writing" mode as
if it had been opened with the `H5F_ACC_SWMR_WRITE` flag set. This is used
when code needs to perform SWMR-forbidden operations like creating groups
and datasets before appending data to datasets using SWMR.

Most of the work of this API call involves flushing out the library caches
in preparation for SWMR access, but there are a few locking operations that
take place under the hood:

- The file's superblock is marked as in the SWMR writer case, above.
- For a brief period of time in the call, we convert the exclusive lock to
  a shared lock. It's unclear why this was done and we'll look into removing
  this.
- At the end of the call, the lock is removed, as in the SWMR write open
  case described above.

## Disabling the locks

There are several ways to disable the locks, depending on which version of the
HDF5 library you are working with. This section will describe the file lock
disable schemes as they exist in late 2022. The current library versions at
this time are 1.10.9, 1.12.3, and 1.13.2. File locks are not present in HDF5
1.8. The lock feature matrix later in this document will describe the
limitations of earlier versions.

### Configure option

You can set the file locking defaults at configure time. This sets the defaults
for the associated properties in the fapl. Users can override the configure
defaults using `H5Pset_file_locking()` or the `HDF5_USE_FILE_LOCKING`
environment variable.

- Autotools

    `--enable-file-locking=(yes|no|best-effort)` sets the file locking behavior.
    `on` and `off` should be self-explanatory. `best-effort` turns file locking
    on but ignores file locks when they are disabled (default: `best-effort`).

- CMake

    - set `IGNORE_DISABLED_FILE_LOCK` to `ON` to ignore file locks when they
      are disabled on the file system (default: `ON`).
    - set `HDF5_USE_FILE_LOCKING` to `OFF` to disable file locks (default: `ON`)

### `H5Pset_file_locking()`

This API call can be used to override the configure defaults. It takes
`hbool_t` parameters for both the file locking and "ignore file locks when
disabled on the file system" parameters. The values set here can be
overridden by the file locking environment variable.

There is a corresponding `H5Pget_file_locking()` call that can be used to check
the currently set values of both properties in the fapl. **NOTE** that this
call just checks the property list values. It does **NOT** check the
environment variables!

### Environment variables

The `HDF5_USE_FILE_LOCKING` environment variable overrides all other file
locking settings.

HDF5 1.10.0
- No file locking environment variable

HDF5 1.10.1 - 1.10.6, 1.12.0:
- `FALSE` turns file locking off
- Anything else turns file locking on
- Neither of these values ignores disabled file locks
- Environment variable parsed at file create/open time

HDF5 1.10.7+, 1.12.1+, 1.13.x:
- `FALSE` or `0` disables file locking
- `TRUE` or `1` enables file locking
- `BEST_EFFORT` enables file locking and ignores disabled file locks
- Anything else gives you the defaults
- Environment variable parsed at library startup

### Lock disable scheme interactions

As mentioned above and reiterated here:
- Configure-time settings set fapl defaults
- `H5Pset_file_locking()` overrides configure-time defaults
- The environment variable setting overrides all

If you want to check that file locking is on, you'll need to check the fapl
setting AND check the environment variable, which can override the fapl.

**!!! WARNING !!!**

Disabling the file locks is at your own risk. If more than one writer process
modifies an HDF5 file at the same time, the file could be corrupted. If a
reader process reads a file that is being modified by a writer, the writer
process might attempt to read garbage and encounter errors or even crash.

In the case of:

- A single process accessing a file with write access
- Any number of processes accessing a file read-only

You can safely disable the file locking scheme.

If you are trying to set up SWMR without the benefit of the file locks, you'll
just need to be extra careful that you hold to rules for SWMR access.

## Feature Matrix

The following table indicates which versions of the library support which file
lock features. 1.13.0 and 1.13.1 are experimental releases (basically glorified
release candidates) so they are not included here.

**Locks**

- P = POSIX locks only, Windows was a no-op that always succeeded
- WP = POSIX and Windows locks
- (-) = POSIX no-op lock fails
- (+) = POSIX no-op lock passes

**Configure Option and Environment Variable**

- on/off = sets file locks on/off
- try = can also set "best effort", where locks are on but ignored if disabled

|Version|Has locks|Configure option|`H5Pset_file_locking()`|`HDF5_USE_FILE_LOCKING`|
|-------|---------|----------------|-----------------------|-----------------------|
|1.8.x|No|-|-|-|
|1.10.0|P(-)|-|-|-|
|1.10.1|P(-)|-|-|on/off|
|1.10.2|P(-)|-|-|on/off|
|1.10.3|P(-)|-|-|on/off|
|1.10.4|P(-)|-|-|on/off|
|1.10.5|P(-)|-|-|on/off|
|1.10.6|P(-)|-|-|on/off|
|1.10.7|P(+)|try|Y|try|
|1.10.8|WP(+)|try|Y|try|
|1.10.9|WP(+)|try|Y|try|
|1.12.0|P(-)|-|-|on/off|
|1.12.1|WP(+)|try|Y|try|
|1.12.2|WP(+)|try|Y|try|
|1.13.2|WP(+)|try|Y|try|


## Appendix: File lock implementation

The file lock system is implemented with `flock(2)` as the archetype since it
has simple semantics and we don't need range locking. Locks are advisory on many
systems, but this shouldn't be a problem for most users since the HDF5 library
always respects them. If you have a program that parses or modifies HDF5 files
independently of the HDF5 library, you'll want to be mindful of any potential
for concurrent access across processes.

On Unix systems, we call `flock()` directly when it's available and pass
`LOCK_SH` (shared lock), `LOCK_EX` (exclusive lock), and `LOCK_UN` (unlock) as
described in the algorithm section. All locks are non-blocking, so we set the
`LOCK_NB` flag. Sadly, `flock(2)` is not POSIX and it doesn't lock files over
NFS. We didn't consider a lack of NFS support a problem since SWMR isn't
supported on networked file systems like NFS (write order preservation isn't
guaranteed) and `flock(2)` usually doesn't fail when you attempt to lock NFS
files.

On Unix systems without `flock(2)`, we implement a scheme based on `fcntl(2)`
(`Pflock()` in `H5system.c`). On these systems we use `F_SETLK` (non-blocking)
as the operation and set `l_type` in `struct flock` to be:

- `F_UNLOCK` for `LOCK_UN`
- `F_WRLOCK` for `LOCK_EX`
- `F_RDLOCK` for `LOCK_SH`

We set the range to be the entire file. Most Unix-like systems have `flock()`
these days, so this system probably isn't very well tested.

We don't use `fcntl`-based open file locks or mandatory locking anywhere. The
former scheme is non-POSIX and the latter is deprecated.

On Windows, we use `LockFileEx()` and `UnlockFileEx()` to lock the entire file
(`Wflock()` in `H5system.c`). We set `LOCKFILE_FAIL_IMMEDIATELY` to get
non-blocking locks and set `LOCKFILE_EXCLUSIVE_LOCK` when we want an exclusive
lock. SWMR isn't well-tested on Windows, so this scheme hasn't been as
thoroughly vetted as the `flock`-based scheme.

On non-Windows systems where neither `flock(2)` nor `fcntl(2)` is available,
we substitute a no-op stub that always succeeds (`Nflock()` in `H5system.c`).
In the past, the stub always failed (see the matrix for when we made the switch).
We currently know of no non-Windows systems where neither call is available
so this scheme is not well-tested.

One thing that should be immediately apparent to anyone familiar with file
locking, is that all of these schemes have subtly different semantics. We're
using file locking in a fairly crude manner, though, and lock use has always
been optional, so we consider this a lower-order concern.

Locks are implemented at the VFD level via `lock` and `unlock` callbacks. The
VFDs that implement file locks are: core (w/ backing store), direct, log, sec2,
and stdio (`flock(2)` locks only). The family, multi, and splitter VFDs invoke
the lock callback of their underlying sub-files. The onion and MPI-IO VFDs do NOT
use locks, even though they create normal, on-disk native HDF5 files. The
read-only S3 VFD and HDFS VFDs do not use file locking since they use
alternative storage schemes.

Lock failures are detected by checking to see if `errno` is set to `ENOSYS`.
This is not particularly sophisticated and was implemented as a way of working
around disabled locks on popular parallel file systems.

One other thing to note here is that, in all of the locking schemes we use, the
file locks do not survive process termination, so you don't have to worry
about files being locked forever if a process exits abnormally. If a writer
crashed and the library didn't clear the superblock mark, you can remove it with
the h5clear command-line tool, which is built with the library.
