#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src directory
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib $(DH5WALK_CPPFLAGS)

# These are our main targets, the tools
# dh5walk_SOURCES=dh5walk.c $(TOOLSOURCES)
bin_PROGRAMS=dh5walk
#bin_SCRIPTS=install-examples

# Add h5stat specific linker flags here
dh5walk_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS) $(DH5WALK_LDFLAGS)

# Tell automake to clean h5redeploy script
CLEANFILES=

# All programs rely on hdf5 library and h5tools library
dh5walk_LDADD=$(LIBH5TOOLS) $(LIBHDF5) $(DH5WALK_LIBS)

include $(top_srcdir)/config/conclude.am
