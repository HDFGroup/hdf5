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
#
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS+=-I$(top_srcdir)/src

bin_PROGRAMS = mirror_server mirror_server_stop

mirror_server_SOURCES = mirror_server.c mirror_writer.c mirror_remote.c
#mirror_writer_SOURCES = mirror_writer.c mirror_remote.c

# All programs depend on the hdf5 library
LDADD=$(LIBHDF5)

include $(top_srcdir)/config/conclude.am
