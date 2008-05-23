#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5-Fortran test/Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include files
AM_FCFLAGS=-I$(top_builddir)/fortran/src $(F9XMODFLAG)$(top_builddir)/fortran/src
INCLUDES=-I$(top_srcdir)/src -I$(top_builddir)/fortran/src

# The Fortran test library
noinst_LTLIBRARIES=libh5test_fortran.la

# Some Fortran compilers can't build shared libraries, so sometimes we
# want to build a shared C library and a static Fortran library.  If so,
# pass the -static flag to the library linker.
if FORTRAN_SHARED_CONDITIONAL
else
   AM_LDFLAGS=-static
endif

# Our main targets, the tests themselves
TEST_PROG=fortranlib_test fflush1 fflush2 fortranlib_test_1_8
check_PROGRAMS=$(TEST_PROG)

libh5test_fortran_la_SOURCES= tf.f90 t.c

# Source files are used for both the library and fortranlib_test.
# Automake will complain about this without the following workaround.
fortranlib_test_FCFLAGS=$(AM_FCFLAGS)
fortranlib_test_CFLAGS=$(AM_CFLAGS)

fortranlib_test_SOURCES = fortranlib_test.f90 \
        tH5F.f90 tH5D.f90 tH5R.f90 tH5S.f90 tH5T.f90 tH5VL.f90 tH5Z.f90 \
        tH5Sselect.f90 tH5P.f90 tH5A.f90 tH5I.f90 tH5G.f90 tH5E.f90

fortranlib_test_1_8_SOURCES = fortranlib_test_1_8.f90 \
        tH5F.f90 tH5O.f90 tH5A_1_8.f90  tH5G_1_8.f90
 
fflush1_SOURCES=fflush1.f90
fflush2_SOURCES=fflush2.f90

# The tests depend on both fortran libraries and both main libraries.
LDADD=libh5test_fortran.la $(LIBH5TEST) $(LIBH5F) $(LIBHDF5)

# Temporary files
CHECK_CLEANFILES+=*.h5
MOSTLYCLEANFILES=*.tmp

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
	  if test -f "$(includedir)/tf.$(F9XMODEXT)" -o -f "$(includedir)/TF.$(F9XMODEXT)"; then \
	    set -x; $(RM) $(includedir)/*.$(F9XMODEXT);                 \
	  fi;                                                           \
	fi;								\
	$(RM) $(bindir)/$(H5FC_NAME)

# Mark this directory as part of the Fortran API (this affects output
# from tests in conclude.am)
FORTRAN_API=yes

# fflush2 depends on files created by fflush1
fflush2.chkexe_: fflush1.chkexe_

# Hardcode the dependencies of these files. There isn't a known way of
# determining this automagically (like we do with the C files). So, when
# doing a parallel make, some modules could be made way before the
# modules they depend upon are actually made. *sigh*
fortranlib_test.lo:             $(srcdir)/fortranlib_test.f90 tf.lo
fortranlib_test_1_8.lo:         $(srcdir)/fortranlib_test_1_8.f90 tf.lo
tH5A_1_8.lo:                    $(srcdir)/tH5A_1_8.f90 tf.lo
tH5G_1_8.lo:                    $(srcdir)/tH5G_1_8.f90 tf.lo
tf.lo:                          $(srcdir)/t.f90

include $(top_srcdir)/config/conclude.am
