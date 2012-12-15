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
##
#
# HDF5-Fortran Examples Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Compile parallel fortran examples only if parallel is enabled
if BUILD_PARALLEL_CONDITIONAL
   EXAMPLE_PROG_PARA=ph5example
endif

# These are the programs that 'make all' or 'make tests' will build and
# that 'make check' will run.  List them in the order in which they should
# be run.
# We don't tell automake about these programs so that it doesn't try to
# compile them with the regular fortran compiler.

EXAMPLE_PROG=h5_crtdat h5_rdwt h5_crtatt h5_crtgrp \
     h5_crtgrpar h5_crtgrpd h5_extend h5_subset h5_cmprss hyperslab selectele \
     refobjexample refregexample mountexample compound

# List files to be installed here
INSTALL_FILES=h5_crtdat.f90 h5_rdwt.f90 \
     h5_crtatt.f90 h5_crtgrp.f90 h5_crtgrpar.f90 h5_crtgrpd.f90  \
     h5_extend.f90 h5_subset.f90 h5_cmprss.f90 hyperslab.f90 selectele.f90 refobjexample.f90 \
     refregexample.f90 mountexample.f90 compound.f90 ph5example.f90
INSTALL_SCRIPT_FILES = run-fortran-ex.sh


# Add attention tests for Fortran 2003 features
if FORTRAN_2003_CONDITIONAL_F
  EXAMPLE_PROG +=  rwdset_fortran2003 nested_derived_type \
      compound_fortran2003 compound_complex_fortran2003
  INSTALL_FILES +=  rwdset_fortran2003.f90 nested_derived_type.f90 \
      compound_fortran2003.f90 compound_complex_fortran2003.f90
endif
TEST_SCRIPT=testh5fc.sh

# Mark this directory as part of the Fortran API
FORTRAN_API=yes

# Some examples depend on files created by other examples.
h5_crtgrpd.chkexe_: h5_crtgrpar.chkexe_
refregexample.chkexe_: refobjexample.chkexe_
# h5_rdwt and h5_crtatt both modify the same file created by
# h5_crtdat. Serialize them.
h5_rdwt.chkexe_: h5_crtdat.chkexe_
h5_crtatt.chkexe_: h5_rdwt.chkexe_

# Tell automake how to build examples using h5fc
# Additional dependencies for the examples are listed below
if BUILD_PARALLEL_CONDITIONAL
$(EXTRA_PROG): $(H5FC_PP)
	$(H5FC_PP) $(H5CCFLAGS) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
else
$(EXTRA_PROG): $(H5FC)
	$(H5FC) $(H5CCFLAGS) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
endif

# Tell automake how to install examples
# Note: no '/' after DESTDIR.  Explanation in commence.am
EXAMPLEDIR=${DESTDIR}$(exec_prefix)/share/hdf5_examples/fortran

# List dependencies for each example.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C compiler, not h5cc.  This is
# an inelegant way of solving the problem.
# All programs share the same build rule and a dependency on the main hdf5
# and fortran libraries above.
h5_crtdat: h5_crtdat.f90
h5_extend: h5_extend.f90
h5_subset: h5_subset.f90
h5_rdwt: h5_rdwt.f90
h5_crtatt: h5_crtatt.f90
h5_crtgrp: h5_crtgrp.f90
h5_crtgrpar: h5_crtgrpar.f90
h5_crtgrpd: h5_crtgrpd.f90
h5_cmprss: h5_cmprss.f90
hyperslab: hyperslab.f90
selectele: selectele.f90
refobjexample: refobjexample.f90
refregexample: refregexample.f90
mountexample: mountexample.f90
compound: compound.f90
ph5example: ph5example.f90
if FORTRAN_2003_CONDITIONAL_F
rwdset_fortran2003: rwdset_fortran2003.f90
nested_derived_type: nested_derived_type.f90
compound_fortran2003: compound_fortran2003.f90
compound_complex_fortran2003: compound_complex_fortran2003.f90
endif

include $(top_srcdir)/config/examples.am
include $(top_srcdir)/config/conclude.am
