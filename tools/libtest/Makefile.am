#
# Read-Only S3 Virtual File Driver (VFD)
# Copyright (c) 2017-2018, The HDF Group.
#
# All rights reserved.
#
# NOTICE:
# All information contained herein is, and remains, the property of The HDF
# Group. The intellectual and technical concepts contained herein are
# proprietary to The HDF Group. Dissemination of this information or
# reproduction of this material is strictly forbidden unless prior written
# permission is obtained from The HDF Group.
##
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
