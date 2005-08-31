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
# hdf5 Parallel Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/test

# Test programs and scripts.  These are our main targets.
#
TEST_PROG_PARA=t_mpi t_fphdf5 testphdf5
TEST_SCRIPT_PARA=testph5.sh

check_PROGRAMS = $(TEST_PROG_PARA)
check_SCRIPTS= $(TEST_SCRIPT)

testphdf5_SOURCES=testphdf5.c t_dset.c t_file.c t_mdset.c t_ph5basic.c \
        t_coll_chunk.c t_span_tree.c t_chunk_alloc.c

# The tests all depend on the hdf5 library and the test library
LDADD = $(LIBHDF5) $(LIBH5TEST)

# Temporary files
# MPItest.h5 is from t_mpi
# Para*.h5 are from testphdf
CHECK_CLEANFILES+=MPItest.h5 Para*.h5
DISTCLEANFILES=go Makefile testph5.sh

include $(top_srcdir)/config/conclude.am
