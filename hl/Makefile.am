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

SUBDIRS=src test $(FORTRAN_DIR)

##lib progs tests test _test:
##	@@SETX@; for d in $(SUBDIRS); do                                      \
##	   (cd $$d && $(MAKE) $@) || exit 1;                                  \
##	done

include $(top_srcdir)/config/conclude.am
