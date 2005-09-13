#
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 Library Performance Makefile(.in)
#

include $(top_srcdir)/config/commence.am

INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/tools/lib

if BUILD_PARALLEL_CONDITIONAL
  TEST_PROG_PARA=h5perf
endif

# h5perf is run as a test in parallel when parallel is available, but it is
# always installed with the tools during make install.
bin_PROGRAMS=h5perf

# These are the programs that `make all' or `make tests' will build and which
# `make check' will run. List them in the order they should be run.
TEST_PROG = iopipe chunk overhead zip_perf perf_meta
check_PROGRAMS=$(TEST_PROG_PARA) $(TEST_PROG) perf

h5perf_SOURCES=pio_perf.c pio_engine.c pio_timer.c

# These are the files that `make clean' (and derivatives) will remove from
# this directory.
CLEANFILES=*.h5 *.raw *.dat x-gnuplot

# All of the programs depend on the main hdf5 library, and some of them
# depend on test or tools library.
LDADD=$(LIBHDF5)
h5perf_LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)
perf_LDADD=$(LIBH5TEST) $(LIBHDF5)
iopipe_LDADD=$(LIBH5TEST) $(LIBHDF5)
zip_perf_LDADD=$(LIBH5TOOLS) $(LIBH5TEST) $(LIBHDF5)
perf_meta_LDADD=$(LIBH5TEST) $(LIBHDF5)

include $(top_srcdir)/config/conclude.am
