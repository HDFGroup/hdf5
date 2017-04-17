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

# Include src and tools/lib directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib

# Test programs and scripts
TEST_PROG=h5importtest
TEST_SCRIPT=h5importtestutil.sh

check_PROGRAMS=$(TEST_PROG)
check_SCRIPT=$(TEST_SCRIPT)
SCRIPT_DEPEND=../../src/h5import/h5import$(EXEEXT)

# All programs depend on the main hdf5 library and the tools library
LDADD=$(LIBH5TOOLS) $(LIBHDF5)

# Temporary files from h5importtest
CHECK_CLEANFILES+=*.bin

include $(top_srcdir)/config/conclude.am
