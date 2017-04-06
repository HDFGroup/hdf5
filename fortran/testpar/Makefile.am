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
# HDF5 Fortran Parallel Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include files
AM_FCFLAGS+=-I$(top_srcdir)/fortran/src -I$(top_srcdir)/fortran/test $(F9XMODFLAG)$(top_builddir)/fortran/src \
            $(F9XMODFLAG)$(top_builddir)/fortran/test

# Some Fortran compilers can't build shared libraries, so sometimes we
# want to build a shared C library and a static Fortran library.  If so,
# pass the -static flag to the library linker.
# (Actually, we historically have bad luck combining shared libraries with
# parallel code.  But you're welcome to try...)
if FORTRAN_SHARED_CONDITIONAL
else
   AM_LDFLAGS+=-static
endif

# These are our main targets
TEST_PROG_PARA=parallel_test
check_PROGRAMS=$(TEST_PROG_PARA)

# Temporary files
CHECK_CLEANFILES+=parf[12].h5

# Test source files
parallel_test_SOURCES=ptest.f90 hyper.f90 mdset.f90

# The tests depend on several libraries.
LDADD=$(LIBH5FTEST) $(LIBH5TEST) $(LIBH5F) $(LIBHDF5)

# Mark this directory as part of the Fortran API
FORTRAN_API=yes

include $(top_srcdir)/config/conclude_fc.am
