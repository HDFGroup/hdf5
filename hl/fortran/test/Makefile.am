#
# Copyright (C) 2001 National Center for Supercomputing Applications.
#                    All rights reserved.
#
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 High-Level Fortran Makefile(.in)

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS=-I$(top_srcdir)/src -I$(top_builddir)/src -I$(top_srcdir)/hl/src
AM_FCFLAGS=-I$(top_builddir)/fortran/src -I$(top_builddir)/hl/fortran/src $(F9XMODFLAG)$(top_builddir)/fortran/src $(F9XMODFLAG)$(top_builddir)/hl/fortran/src

# Fortran libraries are linked statically to solve a build problem.
AM_LDFLAGS=-static

# Our main target, the test programs
TEST_PROG=tstlite tstimage tsttable
check_PROGRAMS=$(TEST_PROG)

LDADD= $(LIBH5F_HL) $(LIBH5F) $(LIBH5_HL)  $(LIBHDF5)

# Source files for the programs
tstlite_SOURCES=tstlite.f90
tstimage_SOURCES=tstimage.f90
tsttable_SOURCES=tsttable.f90

# Temporary files.
MOSTLYCLEANFILES=dsetf[1-4].h5 f1img.h5 f1tab.h5

include $(top_srcdir)/config/conclude.am
