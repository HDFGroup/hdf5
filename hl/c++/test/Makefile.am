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
# HDF5-C++ Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/c++/src -I$(top_srcdir)/hl/src -I$(top_srcdir)/hl/c++/src -I$(top_srcdir)/test -I$(top_builddir)/hl/test -I$(top_srcdir)/hl/test

# These are our main targets.  They should be listed in the order to be
# executed, generally most specific tests to least specific tests.
TEST_PROG=ptableTest
check_PROGRAMS=$(TEST_PROG)

# The tests depend on the hdf5, hdf5 C++, and hdf5_hl libraries
LDADD=$(LIBH5CPP_HL) $(LIBH5_HL) $(LIBH5CPP) $(LIBHDF5)

ptableTest_SOURCES=ptableTest.cpp

# Tell conclude.am that these are C++ tests.
CXX_API=yes

include $(top_srcdir)/config/conclude.am
