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

# Include src and tools/lib directories
INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib -I$(top_srcdir)/hl/src

# These are our main targets, the tools
TEST_SCRIPT=testh5watch.sh
check_SCRIPTS=$(TEST_SCRIPT)
SCRIPT_DEPEND=h5watch$(EXEEXT)

bin_PROGRAMS=h5watch
noinst_PROGRAMS=h5watchgentest extend_dset

# Add h5watch specific linker flags here
h5watch_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

h5watch_SOURCES=h5watch.c

h5watchgentest_SOURCES=h5watchgentest.c
extend_dset_SOURCES=extend_dset.c

# Programs all depend on the hdf5 library, the tools library, and the HL
# library.
LDADD=$(LIBH5_HL) $(LIBH5TOOLS) $(LIBHDF5)
#
CHECK_CLEANFILES+=*.h5
DISTCLEANFILES=testh5watch.sh

include $(top_srcdir)/config/conclude.am
