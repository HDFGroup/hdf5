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
##
#
# Utils HDF5 Makefile(.in)
#

include $(top_srcdir)/config/commence.am

CONFIG=ordered

# All subdirectories
SUBDIRS=mirror_vfd

AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_builddir)/src -I$(top_srcdir)/test

bin_PROGRAMS = vds_check_compat_vol

# All programs depend on the hdf5 library
LDADD=$(LIBH5TEST) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
