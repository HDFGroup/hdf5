#
# Copyright (C) 2001 National Center for Supercomputing Applications.
#                    All rights reserved.
#
# 
# This makefile mostly just reinvokes make in the various subdirectories
# but does so in the correct order.  You can alternatively invoke make from
# each subdirectory manually.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 High-Level Makefile(.in)

include $(top_srcdir)/config/commence.am

## Automake will automatically recurse into fortran directory for distclean
## if we define it conditionally.
if BUILD_FORTRAN_CONDITIONAL
  FORTRAN_DIR = fortran
endif
if BUILD_CXX_CONDITIONAL
  CXX_DIR = c++
endif

SUBDIRS=src test $(CXX_DIR) $(FORTRAN_DIR)

include $(top_srcdir)/config/conclude.am
