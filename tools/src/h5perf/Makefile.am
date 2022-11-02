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
# HDF5 Library Performance Makefile(.in)
#

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/tools/lib

# bin_PROGRAMS will be installed.
if BUILD_PARALLEL_CONDITIONAL
    bin_PROGRAMS=h5perf_serial h5perf
else
    bin_PROGRAMS=h5perf_serial
endif

# Add h5perf and h5perf_serial specific linker flags here
h5perf_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)
h5perf_serial_LDFLAGS = $(LT_STATIC_EXEC) $(AM_LDFLAGS)

# Some programs are not built or run by default, but can be built by hand or by
# specifying --enable-build-all at configure time.
# Also, some of these programs should only be built in parallel.
# Currently there is no such program.
if BUILD_PARALLEL_CONDITIONAL
    PARA_BUILD_ALL=
endif
if BUILD_ALL_CONDITIONAL
    BUILD_ALL_PROGS=$(PARA_BUILD_ALL)
endif

# Define programs that will be run in 'make check'
# List them in the order they should be run.
# Parallel test programs.
if BUILD_PARALLEL_CONDITIONAL
    TEST_PROG_PARA=h5perf
endif

h5perf_SOURCES=pio_perf.c pio_engine.c
h5perf_serial_SOURCES=sio_perf.c sio_engine.c

# All of the programs depend on the main hdf5 library, and some of them
# depend on test or tools library.
LDADD=$(LIBHDF5)
h5perf_LDADD=$(LIBH5TOOLS) $(LIBHDF5)
h5perf_serial_LDADD=$(LIBH5TOOLS) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
