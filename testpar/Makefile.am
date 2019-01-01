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
##
#
# hdf5 Parallel Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/test

# Test scripts--
#       testpflush.sh:
TEST_SCRIPT_PARA = testpflush.sh
SCRIPT_DEPEND = t_pflush1$(EXEEXT) t_pflush2$(EXEEXT) 

check_SCRIPTS = $(TEST_SCRIPT_PARA)

# Test programs.  These are our main targets.
#
TEST_PROG_PARA=t_mpi t_bigio testphdf5 t_cache t_cache_image t_pread t_pshutdown t_prestart t_init_term t_shapesame t_filters_parallel

# t_pflush1 and t_pflush2 are used by testpflush.sh
check_PROGRAMS = $(TEST_PROG_PARA) t_pflush1 t_pflush2

testphdf5_SOURCES=testphdf5.c t_dset.c t_file.c t_file_image.c t_mdset.c \
	t_ph5basic.c t_coll_chunk.c t_span_tree.c t_chunk_alloc.c t_filter_read.c \
	t_prop.c t_coll_md_read.c

# The tests all depend on the hdf5 library and the test library
LDADD = $(LIBH5TEST) $(LIBHDF5)

# Temporary files
# MPItest.h5 is from t_mpi
# Para*.h5 are from testphdf
# bigio_test.h5 is from t_bigio
# ShapeSameTest.h5 is from t_shapesame
# shutdown.h5 is from t_pshutdown
# after_mpi_fin.h5 is from t_init_term
# go is used for debugging. See testphdf5.c.
CHECK_CLEANFILES+=MPItest.h5 Para*.h5 bigio_test.h5 CacheTestDummy.h5 \
		  ShapeSameTest.h5 shutdown.h5  after_mpi_fin.h5 go

include $(top_srcdir)/config/conclude.am
