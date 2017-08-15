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
#
# HDF5-C++ Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src, test, and c++/src directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/test -I$(top_srcdir)/c++/src

# These are our main targets.  They should be listed in the order to be
# executed, generally most specific tests to least specific tests.
TEST_PROG=testhdf5
check_PROGRAMS=$(TEST_PROG)

# The tests depend on the hdf5 library, test library, and the c++ library
LDADD=$(LIBH5TEST) $(LIBH5CPP) $(LIBHDF5)

testhdf5_SOURCES=testhdf5.cpp dsets.cpp tattr.cpp tarray.cpp	\
	tcompound.cpp tdspl.cpp tfile.cpp tfilter.cpp th5s.cpp	\
	tlinks.cpp tobject.cpp trefer.cpp ttypes.cpp tvlstr.cpp	\
	titerate.cpp h5cpputil.cpp

# Tell conclude.am that these are C++ tests.
CXX_API=yes

# Some C++ compilers/linkers (PGI?) create a directory named "ii_files" that
# holds *.ii files, which are template entity instantiations.
# This entire directory should be cleaned.
mostlyclean-local:
	@if test -d ii_files; then                                       \
	  $(RM) -rf ii_files;                                           \
	fi
# Temporary files
CHECK_CLEANFILES+=tattr_multi.h5 tfattrs.h5 tattr_scalar.h5 tattr_compound.h5 \
	tattr_dtype.h5 tattr_basic.h5

include $(top_srcdir)/config/conclude.am
