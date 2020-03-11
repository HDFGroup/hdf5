# VFD SWMR User's Guide

## Caveats

A few library functions are known to work incorrectly with VFD
SWMR.

* Variable-length data written using VFD SWMR may be inaccessible
  or inconsistent to a VFD SWMR reader until the writer closes
  the file.  Instead of the proper variable-length data, a reader may
  read NULL or arbitrary bytes.  Inconsistencies may also cause the
  reader to crash. 

* Applications should take care using HDF5 iteration APIs, especially
  when iterating large numbers of objects or using long-running
  application callbacks.  While the library is in an iteration routine,
  it does not track changes made by the writer.  If the library spends more than
  `max_lag` ticks in the routine, then its view of the HDF5 file will become
  stale.  Under those circumstances, HDF5 content could be mis-read, or the
  library could crash with a diagnostic assertion.

* At the present level of development, the writer cannot invalidate a reader's HDF5 object handles (`hid_t`s).  If a reader holds an object open---that is, it has a valid handle (`hid_t`) for the object---while the writer deletes it, then
  reading content through the handle may yield corrupted data or the data from some
  other object, or the library may crash.
