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

install-examples:
	@echo "Creating demo files" && \
	$(SHELL) $(abs_builddir)/copy_demo_files.sh $(abs_top_srcdir)/tools/test $(abs_top_srcdir)/tools/src/h5diff $(abs_top_srcdir)/tools/src/h5ls;

# Include src directory
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib

TOOLSOURCES=h5tools.c h5tools_dump.c h5tools_str.c h5tools_utils.c h5diff.c \
    h5diff_array.c h5diff_attr.c h5diff_dset.c h5diff_util.c h5trav.c  \
    h5tools_filters.c h5tools_ref.c h5tools_type.c io_timer.c

# These are our main targets, the tools
# dh5walk_SOURCES=dh5walk.c $(TOOLSOURCES)
bin_PROGRAMS=dh5walk
# bin_SCRIPTS=install-examples
bin_SCRIPTS=

# Add h5stat specific linker flags here
dh5walk_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

# Tell automake to clean h5redeploy script
CLEANFILES=

# All programs rely on hdf5 library and h5tools library
dh5walk_LDADD=$(LIBH5TOOLS) $(LIBHDF5) $(LIBMFU)
# dh5walk_LDADD=$(LIBHDF5) $(LIBMFU)

include $(top_srcdir)/config/conclude.am
