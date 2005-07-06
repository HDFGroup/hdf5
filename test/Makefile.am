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
# HDF5 Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

INCLUDES=-I$(top_srcdir)/src -I$(top_builddir)/src

# Test script for error_test and err_compat
TEST_SCRIPT = $(top_srcdir)/test/testerror.sh
check_SCRIPTS = $(TEST_SCRIPT)


# These are our main targets. They should be listed in the order to be
# executed, generally most specific tests to least specific tests.
TEST_PROG=testhdf5 lheap ohdr stab gheap cache b+tree btree2 blocktrack sheap \
           pool hyperslab istore bittests dt_arith \
           dtypes dsets cmpd_dset extend external links unlink big mtime     \
           fillval mount flush1 flush2 enum \
           set_extent ttsafe stream_test \
           getname vfd ntypes dangle dtransform filename reserved 

# List programs to be built when testing here. error_test and err_compat are
# built at the same time as the other tests, but executed by testerror.sh.
# 'make check' doesn't run them directly, so they are not included in TEST_PROG.
# Also build testmeta, which is used for timings test.  It builds quickly,
# and this lets automake keep all its test programs in one place.
check_PROGRAMS=$(TEST_PROG) error_test err_compat testmeta


# The libh5test.a library provides common support code for the tests.
check_LTLIBRARIES=libh5test.la
libh5test_la_SOURCES=h5test.c testframe.c
libh5test_la_LIBADD=$(LIBHDF5)

# Build the library statically because some compilers can only link
# one dynamic library and the main hdf5 library is much larger than this
# one.
libh5test_la_LDFLAGS=-static

# Use libhd5test.la to compile all of the tests
LDADD=libh5test.la $(LIBHDF5)

# List the source files for tests that have more than one
ttsafe_SOURCES=ttsafe.c ttsafe_dcreate.c ttsafe_error.c ttsafe_cancel.c       \
               ttsafe_acreate.c

# Additional target for running timing test
timings _timings: testmeta
	@for timing in $(TIMINGS) dummy; do                                   \
	   if test $$timing != dummy; then                                    \
	      echo "Running $$timing $(TEST_FLAGS)";                          \
	      $(RUNTEST) ./$$timing $(TEST_FLAGS) || exit 1;                  \
	   fi;                                                                \
	done;


# Temporary files.  These files are the ones created by setting the
# HDF5_NOCLEANUP environment variable and running `make test' without
# specifying a file prefix or low-level driver.  Changing the file
# prefix or low-level driver with environment variables will influence 
# the temporary file name in ways that the makefile is not aware of.
MOSTLYCLEANFILES+=cmpd_dset.h5 compact_dataset.h5 dataset.h5 extend.h5 istore.h5\
            tfile[1-4].h5 th5s[1-3].h5 lheap.h5 ohdr.h5 stab[1-2].h5 \
            extern_[1-3].h5 extern_[1-4][ab].raw gheap[0-4].h5 dt_arith[1-2]\
            links.h5 links[1-3].h5 big.data big[0-9][0-9][0-9][0-9][0-9].h5  \
            dtypes[1-8].h5 dt_arith[1-2].h5 tattr.h5 tselect.h5 mtime.h5 \
            unlink.h5 unicode.h5 \
            fillval_[0-9].h5 fillval.raw mount_[0-9].h5 testmeta.h5 ttime.h5 \
            trefer[1-3].h5 tvltypes.h5 tvlstr.h5 tvlstr2.h5 flush.h5         \
            enum1.h5 titerate.h5 ttsafe.h5 tarray1.h5 tgenprop.h5            \
            tmisc[0-9]*.h5 set_extent_read.h5 set_extent_create.h5           \
            getname.h5 getname[1-3].h5 sec2_file.h5                          \
            family_file000[0-3][0-9].h5 multi_file-[rs].h5 core_file         \
            new_move_[ab].h5 ntypes.h5 dangle.h5 error_test.h5 err_compat.h5 \
            dtransform.h5 test_filters.h5 get_file_name.h5 tstint[1-2].h5    \
            unlink_chunked.h5 btree2.h5 blocktrack.h5 sheap.h5 b+tree.h5

# Sources for testhdf5 executable
testhdf5_SOURCES=testhdf5.c tarray.c tattr.c tconfig.c tfile.c tgenprop.c \
    th5s.c theap.c tid.c titerate.c tmeta.c tmisc.c ttime.c trefer.c trefstr.c  \
    tselect.c tskiplist.c ttst.c tunicode.c tvltypes.c tvlstr.c

include $(top_srcdir)/config/conclude.am
