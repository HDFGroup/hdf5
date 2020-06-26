#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src, test, and tools/lib directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/tools/lib

# A convenience library for the h5repack tool and the h5repack tests
noinst_LTLIBRARIES=libh5repack.la

libh5repack_la_SOURCES=h5repack.c h5repack_copy.c h5repack_filters.c \
                       h5repack_opttable.c h5repack_parse.c h5repack_refs.c \
                       h5repack_verify.c
libh5repack_la_LDFLAGS = $(AM_LDFLAGS)
libh5repack_la_LIBADD=$(LIBH5TOOLS) $(LIBHDF5)


# Our main target, h5repack tool
bin_PROGRAMS=h5repack

h5repack_SOURCES=h5repack_main.c

# Add h5repack specific linker flags here
h5repack_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

# Depend on the hdf5 library, the tools library, the h5repack library
h5repack_LDADD=libh5repack.la $(LIBH5TOOLS) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
