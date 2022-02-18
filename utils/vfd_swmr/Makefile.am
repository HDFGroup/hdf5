#
# Copyright by Akadio, Inc.
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

AM_CPPFLAGS+=-I$(top_srcdir)/src

bin_PROGRAMS = aux_process

aux_process_SOURCES = aux_process.c

# All programs depend on the hdf5 library
# LDADD=$(LIBHDF5)

include $(top_srcdir)/config/conclude.am
