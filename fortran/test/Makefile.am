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
# Autoconf cannot figure out dependencies between modules; disable parallel make
.NOTPARALLEL:
include $(top_srcdir)/config/commence.am

# Include files
AM_FCFLAGS+=-I$(top_builddir)/fortran/src $(F9XMODFLAG)$(top_builddir)/fortran/src
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_builddir)/fortran/src

# The Fortran test library
noinst_LTLIBRARIES=libh5test_fortran.la

# Some Fortran compilers can't build shared libraries, so sometimes we
# want to build a shared C library and a static Fortran library.  If so,
# pass the -static flag to the library linker.
if FORTRAN_SHARED_CONDITIONAL
else
   AM_LDFLAGS+=-static
endif

# Our main targets, the tests themselves
TEST_PROG=fortranlib_test fflush1 fflush2 fortranlib_test_1_8 fortranlib_test_F03

check_PROGRAMS=$(TEST_PROG)

libh5test_fortran_la_SOURCES = tf_gen.F90 tf.F90  t.c

fortranlib_test_SOURCES = tH5F.F90 tH5D.F90 tH5R.F90 tH5S.F90 tH5T.F90 tH5VL.F90 tH5Z.F90 \
        tH5Sselect.F90 tH5P.F90 tH5A.F90 tH5I.F90 tH5G.F90 tH5E.F90 tHDF5.F90 fortranlib_test.F90

fortranlib_test_1_8_SOURCES = tH5O.F90 tH5A_1_8.F90 tH5G_1_8.F90 tH5MISC_1_8.F90 tHDF5_1_8.F90 \
        fortranlib_test_1_8.F90

fortranlib_test_F03_SOURCES = tH5E_F03.F90 tH5F_F03.F90 tH5L_F03.F90 \
        tH5O_F03.F90 tH5P_F03.F90 tH5T_F03.F90 tHDF5_F03.F90 fortranlib_test_F03.F90


fflush1_SOURCES=fflush1.F90
fflush2_SOURCES=fflush2.F90

# The tests depend on both fortran libraries and both main libraries.
LDADD=libh5test_fortran.la $(LIBH5TEST) $(LIBH5F) $(LIBHDF5)

# Temporary files
CHECK_CLEANFILES+=*.h5 *.raw
MOSTLYCLEANFILES=*.tmp

# Fortran module files can have different extensions and different names
# (e.g., different capitalizations) on different platforms.  Write rules
# for them explicitly rather than trying to teach automake about them.
# They should be installed as headers and removed during clean.
maintainer-clean-local: clean-local
distclean-local: clean-local
clean-local:
	@if test -n "$(F9XMODEXT)" && test "X$(F9XMODEXT)" != "Xo"; then \
	  $(RM) *.$(F9XMODEXT) tf_gen.F90;                                         \
	fi

# Mark this directory as part of the Fortran API (this affects output
# from tests in conclude.am)
FORTRAN_API=yes

# helper program we need to build.
noinst_PROGRAMS = H5_test_buildiface

# H5_test_buildiface.F90 generates all the test APIs that have a KIND type associated
# with them.

tf_gen.F90: H5_test_buildiface$(EXEEXT)
	$(RUNSERIAL) ./H5_test_buildiface$(EXEEXT)

# H5_test_buildiface.F90 is included in the distribution, and Automake knows
# how to compile a fortran program given its sources.

H5_test_buildiface_SOURCES = H5_test_buildiface.F90

# The build of the H5_test_buildiface does not depend on any libraries, so set it 
# to nothing.

H5_test_buildiface_LDADD =

# fflush2 depends on files created by fflush1
fflush2.chkexe_: fflush1.chkexe_


include $(top_srcdir)/config/conclude_fc.am
