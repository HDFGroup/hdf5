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
# HDF5 Library Examples Makefile(.in)
#

include $(top_srcdir)/config/commence.am

INSTALL_SCRIPT_FILES = run-c++-ex.sh

# These are the programs that 'make all' or 'make prog' will build and
# which 'make check' will run.  List them in the order they should be run.
EXAMPLE_PROG=create readdata writedata compound extend_ds chunks h5group \
	     h5tutr_cmprss h5tutr_crtatt h5tutr_crtdat h5tutr_crtgrpar \
	     h5tutr_crtgrp h5tutr_crtgrpd h5tutr_extend h5tutr_rdwt \
	     h5tutr_subset
TEST_SCRIPT=testh5c++.sh
TEST_EXAMPLES_SCRIPT=$(INSTALL_SCRIPT_FILES)

# These are the example files to be installed
INSTALL_FILES=create.cpp readdata.cpp writedata.cpp compound.cpp \
              extend_ds.cpp chunks.cpp h5group.cpp \
	      h5tutr_cmprss.cpp h5tutr_crtatt.cpp h5tutr_crtdat.cpp \
	      h5tutr_crtgrpar.cpp h5tutr_crtgrp.cpp h5tutr_crtgrpd.cpp \
	      h5tutr_extend.cpp h5tutr_rdwt.cpp h5tutr_subset.cpp

# Some of the examples depend on files created by running other examples
readdata.chkexe_: create.chkexe_
chunks.chkexe_: extend_ds.chkexe_
h5tutr_rdwt.chkexe_: h5tutr_crtdat.chkexe
h5tutrcrtatt.chkexe_: h5tutr_crtdat.chkexe
h5tutr_crtgrpd.chkexe_: h5tutr_crtgrpar.chkexe

# Tell conclude.am that these are C++ tests.
CXX_API=yes

# Where to install examples
# Note: no '/' after DESTDIR.  Explanation in commence.am
EXAMPLEDIR=${DESTDIR}$(exec_prefix)/share/hdf5_examples/c++
EXAMPLETOPDIR=${DESTDIR}$(exec_prefix)/share/hdf5_examples

# How to build programs using h5c++
$(EXTRA_PROG): $(H5CPP)
	$(H5CPP) $(H5CCFLAGS) $(CPPFLAGS) -o $@ $(srcdir)/$@.cpp

# List dependencies for each program.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C++ compiler, not h5c++.  This is
# an inelegant way of solving the problem, unfortunately.
create: $(srcdir)/create.cpp
readdata: $(srcdir)/readdata.cpp
writedata: $(srcdir)/writedata.cpp
compound: $(srcdir)/compound.cpp
extend_ds: $(srcdir)/extend_ds.cpp
chunks: $(srcdir)/chunks.cpp
h5group: $(srcdir)/h5group.cpp

h5tutr_cmprss: $(srcdir)/h5tutr_cmprss.cpp
h5tutr_crtatt: $(srcdir)/h5tutr_crtatt.cpp
h5tutr_crtdat: $(srcdir)/h5tutr_crtdat.cpp
h5tutr_crtgrpar: $(srcdir)/h5tutr_crtgrpar.cpp
h5tutr_crtgrp: $(srcdir)/h5tutr_crtgrp.cpp
h5tutr_crtgrpd: $(srcdir)/h5tutr_crtgrpd.cpp
h5tutr_extend: $(srcdir)/h5tutr_extend.cpp
h5tutr_rdwt: $(srcdir)/h5tutr_rdwt.cpp
h5tutr_subset: $(srcdir)/h5tutr_subset.cpp

include $(top_srcdir)/config/examples.am
include $(top_srcdir)/config/conclude.am
