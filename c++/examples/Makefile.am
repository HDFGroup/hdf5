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
# HDF5 Library Examples Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Use h5c++ to build examples, instead of reguar C++ compiler
H5CPP=$(bindir)/h5c++

# These are the programs that 'make all' or 'make prog' will build and
# which 'make check' will run.  List them in the order they should be run.
TEST_PROG=create readdata writedata compound extend_ds chunks h5group

EXAMPLE_PROG=$(TEST_PROG)

# Tell conclude.in when to build the examples
EXTRA_PROG=$(TEST_PROG)

# These are the example files to be installed
INSTALL_FILES=create.cpp readdata.cpp writedata.cpp compound.cpp         \
              extend_ds.cpp chunks.cpp h5group.cpp

# Some of the examples depend on files created by running other examples
readdata.chkexe_: create.chkexe_
chunks.chkexe_: extend_ds.chkexe_

# These are the files that 'make clean' (and derivatives) will remove from this
# directory.  Since automake doesn't know about the examples, we need to
# tell it to clean the example programs, too.
CHECK_CLEANFILES+=*.h5
MOSTLYCLEANFILES=$(EXTRA_PROG:=.o)
CLEANFILES=$(EXTRA_PROG)

# Tell conclude.am that these are C++ tests.
HDF_CXX=yes

# How to install examples
EXAMPLEDIR=$(docdir)/hdf5/examples/c++

$(EXAMPLEDIR):
	-$(top_srcdir)/bin/mkdirs $@

install-data-local:
	@$(MAKE) install-examples
uninstall-local:
	@$(MAKE) uninstall-examples

# Some C++ compilers/linkers will create a directory named ii_files,
# which should be cleaned.
mostlyclean-local:
	if test -d ii_files; then                                       \
	  $(RM) -rf ii_files;                                           \
	fi

# Install source files, which are the examples with .cpp suffixes.  Do the same
# for uninstall.
install-examples: $(EXAMPLEDIR)
	@for f in X $(INSTALL_FILES); do                                      \
	  if test $$f != X; then                                              \
	    (set -x; $(INSTALL_DATA) $(top_srcdir)/c++/examples/$$f $(EXAMPLEDIR)/. || exit 1);\
	  fi;                                                                 \
	done

uninstall-examples:
	@if test -n "$(INSTALL_FILES)" -a -d $(EXAMPLEDIR); then              \
	  set -x; cd $(EXAMPLEDIR) && $(RM) $(INSTALL_FILES).cpp;             \
	fi

check-install:
	@$(MAKE) $(AM_MAKEFLAGS) installcheck
installcheck-local:
	@$(MAKE) $(AM_MAKEFLAGS) check

# How to build programs using h5c++
$(EXTRA_PROG): $(H5CPP) $(LIBHDF5) $(LIBH5CPP)
	$(H5CPP) $(CPPFLAGS) -o $@ $(srcdir)/$@.cpp

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

include $(top_srcdir)/config/conclude.am
