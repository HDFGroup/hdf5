# VFD SWMR User's Guide

## Caveats

A few library functions are known to work incorrectly with VFD
SWMR.

* Variable-length data written using VFD SWMR may be inaccessible
  or inconsistent to a VFD SWMR reader until the writer closes
  the file.  Instead of the proper variable-length data, a reader may
  read NULL or arbitrary bytes.  Inconsistencies may also cause the
  reader to crash. 
