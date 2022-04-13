#
# Copyright by The HDF Group.
# All rights reserved.
#
# NOTICE:
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src and tools/lib directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib

# All programs depend on the hdf5 and h5tools libraries
LDADD=$(LIBH5TOOLS) $(LIBHDF5)


# main target
bin_PROGRAMS=h5tools_test_utils
# check_PROGRAMS=$(TEST_PROG)


include $(top_srcdir)/config/conclude.am
