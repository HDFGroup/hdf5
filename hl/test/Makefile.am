# Copyright (C) 2001 National Center for Supercomputing Applications.
#                    All rights reserved.
#
#
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 High-Level Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Add include directories to C preprocessor flags
AM_CPPFLAGS=-I. -I$(srcdir) -I$(top_builddir)/src -I$(top_srcdir)/src -I$(top_srcdir)/hl/src

# The tests depend on the hdf5 and hdf5_hl libraries
LDADD=$(LIBH5_HL) $(LIBHDF5)

# Test programs.  These are our main targets.  They should be listed in the
# order to be executed, generally most specific tests to least specific tests.
TEST_PROG=test_lite test_image test_table test_ds test_packet
check_PROGRAMS=$(TEST_PROG)

# Temporary files.  These files are the ones created by running `make test'.
CHECK_CLEANFILES+=combine_tables[1-2].h5 test_ds[1-6].h5 test_image[1-3].h5 \
        test_lite[1-2].h5 test_table.h5 test_packet_table.h5

include $(top_srcdir)/config/conclude.am
