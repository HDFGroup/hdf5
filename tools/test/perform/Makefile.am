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
    TEST_PROG_PARA=
endif
# Serial test programs.
TEST_PROG = iopipe chunk chunk_cache overhead zip_perf perf_meta $(BUILD_ALL_PROGS)

# check_PROGRAMS will be built but not installed.  Do not any executable
# that is in bin_PROGRAMS already. Otherwise, it will be removed twice in
# "make clean" and some systems, e.g., AIX, do not like it.
check_PROGRAMS= $(TEST_PROG) $(BUILD_ALL_PROGS)

# These are the files that `make clean' (and derivatives) will remove from
# this directory.
CLEANFILES=*.h5 *.raw *.dat x-gnuplot perftest.out

# All of the programs depend on the main hdf5 library, and some of them
# depend on test or tools library.
LDADD=$(LIBHDF5)
iopipe_LDADD=$(LIBH5TEST) $(LIBHDF5)
zip_perf_LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)
perf_meta_LDADD=$(LIBH5TEST) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
