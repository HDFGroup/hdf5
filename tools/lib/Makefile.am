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

# Include files in /src directory
AM_CPPFLAGS+=-I$(top_srcdir)/src

# This is our main target, the h5tools library.
noinst_LTLIBRARIES=libh5tools.la

libh5tools_la_SOURCES=h5tools.c h5tools_dump.c h5tools_str.c h5tools_utils.c h5diff.c \
    h5diff_array.c h5diff_attr.c h5diff_dset.c h5diff_util.c h5trav.c  \
    h5tools_filters.c h5tools_ref.c h5tools_type.c io_timer.c

# Test program.  Link using libhdf5 and libh5tools
TEST_PROG=
check_PROGRAMS=$(TEST_PROG)

# Name libh5tools.la so that dependencies work out. Automake knows how
# to build 'libh5tools.la', but not '../../tools/lib/libh5tools.la'.
LDADD=libh5tools.la $(LIBHDF5)

include $(top_srcdir)/config/conclude.am

