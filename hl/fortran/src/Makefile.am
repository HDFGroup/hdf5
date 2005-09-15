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

INCLUDES=-I$(top_srcdir)/src -I$(top_srcdir)/hl/src -I$(top_builddir)/hl/src \
         -I$(top_srcdir)/fortran/src -I$(top_builddir)/fortran/src
AM_FCFLAGS=-I$(top_builddir)/fortran/src $(F9XMODFLAG)$(top_builddir)/fortran/src

# Our main target, the high-level fortran library
lib_LTLIBRARIES=libhdf5hl_fortran.la

# Some Fortran compilers can't build shared libraries, so sometimes we
# want to build a shared C library and a static Fortran library.  If so,
# pass the -static flag to the library linker.
if FORTRAN_SHARED_CONDITIONAL
else
   AM_LDFLAGS=-static
endif

# Source files for the library
#if BUILD_PARALLEL_CONDITIONAL
#    PARALLEL_COND_SRC=HDFDmpiof.c HDF5mpio.f90
#endif

libhdf5hl_fortran_la_SOURCES=H5LTfc.c  H5IMfc.c H5IMcc.c H5TBfc.c H5LTff.f90  \
                H5IMff.f90 H5TBff.f90 

# Fortran module files can have different extensions and different names
# (e.g., different capitalizations) on different platforms.  Write rules
# for them explicitly rather than trying to teach automake about them.
# They should be installed as headers and removed during clean.
maintainer-clean-local: clean-local
distclean-local: clean-local
clean-local:
	@if test -n "$(F9XMODEXT)" && test "X$(F9XMODEXT)" != "Xo"; then \
	  $(RM) *.$(F9XMODEXT);                                         \
	fi

install-data-local:
	@if test -n "$(F9XMODEXT)" && test "X$(F9XMODEXT)" != "Xo"; then \
	  $(CP) $(top_builddir)/$(subdir)/*.$(F9XMODEXT) $(includedir)/. ; \
	fi

uninstall-local:
	@if test -n "$(F9XMODEXT)" -a "X$(F9XMODEXT)" != "Xo"; then      \
	  if test -f "$(includedir)/hdf5.$(F9XMODEXT)" -o -f "$(includedir)/HDF5.$(F9XMODEXT)"; then \
	    set -x; $(RM) $(includedir)/*.$(F9XMODEXT);                 \
	  fi;                                                           \
	fi

# Hardcode the dependencies of these files. There isn't a known way of
# determining this automagically (like we do with the C files). So, when
# doing a parallel make, some modules could be made way before the
# modules they depend upon are actually made. *sigh*
H5LTff.lo:          $(srcdir)/H5LTff.f90 
H5IMff.lo:          $(srcdir)/H5IMff.f90
H5TBff.lo:          $(srcdir)/H5TBff.f90 

include $(top_srcdir)/config/conclude.am
