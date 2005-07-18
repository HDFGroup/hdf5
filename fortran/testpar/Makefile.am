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
#
# HDF5 Fortran Parallel Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include files
AM_FCFLAGS=-I$(top_srcdir)/fortran/src -I$(top_srcdir)/fortran/test $(F9XMODFLAG)$(top_builddir)/fortran/src

# Fortran libraries are linked statically to solve a build problem.
AM_LDFLAGS=-static

# These are our main targets
TEST_PROG_PARA=parallel_test
check_PROGRAMS=$(TEST_PROG_PARA)

# Temporary files
MOSTLYCLEAN=ParaEg[123].h5f
DISTCLEAN=go

# Test source files
parallel_test_SOURCES=ptest.f90 hyper.f90 mdset.f90

# The tests depend on several libraries.
LDADD=$(LIBH5FTEST) $(LIBH5TEST) $(LIBH5F) $(LIBHDF5)

# Mark this directory as part of the Fortran API
HDF_FORTRAN=yes

# Put in dependencies to the THDF5 module so that things will be built
# in the correct order.
#thdf5.lo:		   thdf5.o
#thyperslab_wr.o:           $(srcdir)/thyperslab_wr.f90 thdf5.lo
#ptesthdf5_fortran.o:       $(srcdir)/ptesthdf5_fortran.f90 thdf5.lo

include $(top_srcdir)/config/conclude.am
