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

# Include src directory
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib -I$(top_srcdir)/tools/test/misc/vds

#test scripts and programs
TEST_PROG=UC_1_one_dim_gen UC_2_two_dims_gen UC_3_gaps_gen UC_4_printf_gen \
    UC_5_stride_gen

check_PROGRAMS=$(TEST_PROG)

# Temporary files.
CHECK_CLEANFILES+=*.h5

# All programs rely on hdf5 library and h5tools library
LDADD=$(LIBH5TOOLS) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
