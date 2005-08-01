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
# HDF5-Fortran Examples Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Compile parallel fortran examples only if parallel is enabled
if BUILD_PARALLEL_CONDITIONAL
   TEST_PROG_PARA=ph5example
endif

# These are the programs that 'make all' or 'make tests' will build and
# that 'make check' will run.  List them in the order in which they should
# be run.
# We don't tell automake about these programs so that it doesn't try to
# compile them with the regular fortran compiler.
TEST_PROG=dsetexample fileexample rwdsetexample attrexample groupexample    \
       grpsexample grpdsetexample hyperslab selectele grpit refobjexample   \
       refregexample mountexample compound

# Tell configure.in when to build examples
EXTRA_PROG=$(TEST_PROG) $(TEST_PROG_PARA)
EXAMPLE_PROG=$(EXTRA_PROG)

# List files to be installed here
INSTALL_FILES=dsetexample.f90 fileexample.f90 rwdsetexample.f90             \
       attrexample.f90 groupexample.f90 grpsexample.f90 grpdsetexample.f90  \
       hyperslab.f90 selectele.f90 grpit.f90 refobjexample.f90              \
       refregexample.f90 mountexample.f90 compound.f90 ph5example.f90

# Tell automake which files to clean
MOSTLYCLEANFILES+=$(EXTRA_PROG:=.o)
CLEANFILES=*.h5 $(EXTRA_PROG)

# Mark this directory as part of the Fortran API
HDF_FORTRAN=yes

# Some examples depend on files created by other examples.
grpdsetexample.chkexe_: grpsexample.chkexe_
refregexample.chkexe_: refobjexample.chkexe_
# rwdsetexample and attrexample both modify the same file created by
# dsetexample. Serialize them.
rwdsetexample.chkexe_: dsetexample.chkexe_
attrexample.chkexe_: rwdsetexample.chkexe_

# Tell automake how to build examples using h5fc
# Additional dependencies for the examples are listed below
$(EXTRA_PROG): $(LIBHDF5) $(LIBH5F)
if BUILD_PARALLEL_CONDITIONAL
	$(H5FC_PP) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
else
	$(H5FC) $(FCFLAGS) -o $@ $(srcdir)/$@.f90;
endif

# Tell automake how to install examples
EXAMPLEDIR=$(docdir)/hdf5/examples/fortran

$(EXAMPLEDIR):
	-$(top_srcdir)/bin/mkdirs $@

install-data-local:
	@$(MAKE) install-examples
uninstall-local:
	@$(MAKE) uninstall-examples

# We're installing source files, not the example programs.  Add .f90 to each
# example program to get its source (and do the same when uninstalling).
install-examples: $(EXAMPLEDIR)
	@for f in X $(INSTALL_FILES); do                                   \
	  if test $$f != X; then                                           \
	    (set -x; $(INSTALL_DATA) $(top_srcdir)/fortran/examples/$$f $(EXAMPLEDIR)/. || exit 1);\
	  fi;                                                              \
	done

uninstall-examples:
	@if test -n "$(INSTALL_FILES)" -a -d $(EXAMPLEDIR); then                               \
	  set -x; cd $(EXAMPLEDIR) && $(RM) $(EXAMPLE_PROG);               \
	fi

check-install:
	@$(MAKE) installcheck
installcheck-local:
	@$(MAKE) check

# List dependencies for each example.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C compiler, not h5cc.  This is
# an inelegant way of solving the problem.
# All programs share the same build rule and a dependency on the main hdf5
# and fortran libraries above.
dsetexample: dsetexample.f90
fileexample: fileexample.f90
rwdsetexample: rwdsetexample.f90
attrexample: attrexample.f90
groupexample: groupexample.f90
grpsexample: grpsexample.f90
grpdsetexample: grpdsetexample.f90
hyperslab: hyperslab.f90
selectele: selectele.f90
grpit: grpit.f90
refobjexample: refobjexample.f90
refregexample: refregexample.f90
mountexample: mountexample.f90
compound: compound.f90
ph5example: ph5example.f90

include $(top_srcdir)/config/conclude.am
