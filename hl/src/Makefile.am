# Copyright (C) 2001 National Center for Supercomputing Applications.
#                    All rights reserved.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 High-Level Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Add include directories to the C preprocessor flags
#AM_CPPFLAGS=-I. -I$(srcdir) -I$(top_builddir)/src -I$(top_srcdir)/src -I$(top-srcdir)/tools/lib
AM_CPPFLAGS=-I$(top_srcdir)/src

# This library is our main target.
lib_LTLIBRARIES=libhdf5_hl.la

libhdf5_hl_la_SOURCES=H5LT.c H5TB.c H5IM.c H5DS.c

# Public header files (to be installed)
include_HEADERS=H5IM.h H5LT.h H5TB.h H5DS.h

include $(top_srcdir)/config/conclude.am
