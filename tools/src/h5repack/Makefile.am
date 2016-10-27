#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src, test, and tools/lib directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/tools/lib

# Our main target, h5repack tool
bin_PROGRAMS=h5repack

# Add h5repack specific linker flags here
h5repack_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

# Depend on the hdf5 library, the tools library, the test library
LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)

# Source files
COMMON_SOURCES=h5repack.c h5repack_copy.c h5repack_filters.c \
               h5repack_opttable.c h5repack_parse.c h5repack_refs.c \
               h5repack_verify.c

h5repack_SOURCES=$(COMMON_SOURCES) h5repack_main.c

include $(top_srcdir)/config/conclude.am
