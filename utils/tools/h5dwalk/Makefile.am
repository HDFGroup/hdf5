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
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib $(H5DWALK_CPPFLAGS)

# These are our main targets, the tools
# h5dwalk_SOURCES=h5dwalk.c $(TOOLSOURCES)
bin_PROGRAMS=h5dwalk
#bin_SCRIPTS=install-examples

# Add h5stat specific linker flags here
h5dwalk_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS) $(H5DWALK_LDFLAGS)

# Tell automake to clean h5redeploy script
CLEANFILES=

# All programs rely on hdf5 library and h5tools library
h5dwalk_LDADD=$(LIBH5TOOLS) $(LIBHDF5) $(H5DWALK_LIBS)

include $(top_srcdir)/config/conclude.am
