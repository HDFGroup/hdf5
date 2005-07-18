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

if BUILD_PARALLEL_CONDITIONAL
  TEST_PROG_PARA = ph5example
endif

# Example programs.
# Don't tell automake about them, because if it knew they were programs,
# it would try to compile them instead of using the h5cc script.
# This creates some extra work for us.
#
# It might be possible to use automake's COMPILE variable to have it
# use h5cc properly (and h5fc and h5c++).
TEST_PROG = h5_write h5_read h5_extend_write h5_chunk_read h5_compound   \
           h5_group h5_select h5_attribute h5_mount h5_reference h5_drivers

# Tell conclude.in boilerplate when to build the examples
EXAMPLE_PROG = $(TEST_PROG_PARA) $(TEST_PROG)
EXTRA_PROG = $(EXAMPLE_PROG)

# Install files
# List all file that should be installed in examples directory
INSTALL_FILES = h5_write.c h5_read.c h5_extend_write.c h5_chunk_read.c   \
          h5_compound.c h5_group.c h5_select.c h5_attribute.c h5_mount.c \
          h5_reference.c h5_drivers.c ph5example.c

# We need to tell automake what to clean
MOSTLYCLEANFILES+=*.h5 *.raw *.meta *.clog *.o
CLEANFILES=$(TEST_PROG) $(TEST_PROG_PARA)

# Additional dependencies for each program are listed below.
$(EXTRA_PROG): $(LIBHDF5)
if BUILD_PARALLEL_CONDITIONAL
	$(H5CC_PP) $(CFLAGS) -o $@ $(srcdir)/$@.c;
else
	$(H5CC) $(CFLAGS) -o $@ $(srcdir)/$@.c;
endif

# Two of the examples depend on files created by other examples.
h5_read.chkexe_: h5_write.chkexe_
h5_chunk_read.chkexe_: h5_extend_write.chkexe_

# How to install examples
# There are a lot of rules because automake does not know about
# the examples programs, and does not recognize the install-examples
# target.
EXAMPLEDIR=$(docdir)/hdf5/examples/c

$(EXAMPLEDIR):
	-$(top_srcdir)/bin/mkdirs $@

install-data-local:
	$(MAKE) $(AM_MAKEFLAGS) install-examples
uninstall-local:
	$(MAKE) $(AM_MAKEFLAGS) uninstall-examples

# We want to install the source files, not the examples themselves.  Add '.c' to
# each example to get its source.  This is a bit kludgy.
install-examples: $(EXAMPLEDIR) $(INSTALL_FILES)
	@for f in X $(INSTALL_FILES); do                                     \
	  if test $$f != X; then                                             \
	    (set -x; $(INSTALL) $(top_srcdir)/examples/$$f $(EXAMPLEDIR)/. || exit 1);\
	  fi;                                                                \
	done

uninstall-examples:
	@if test -n "$(INSTALL_FILES)" -a -d $(EXAMPLEDIR); then                                 \
	  set -x; cd $(EXAMPLEDIR) && $(RM) $(INSTALL_FILES);              \
	fi

check-install:
	$(MAKE) $(AM_MAKEFLAGS) installcheck
installcheck-local:
	$(MAKE) $(AM_MAKEFLAGS) check

# List dependencies for each program.  Normally, automake would take
# care of this for us, but if we tell automake about the programs it
# will try to build them with the normal C compiler, not h5cc.  This is
# an inelegant way of solving the problem.
# All programs share the same build rule and a dependency on the main hdf5
# library above.
h5_chunk_read: $(srcdir)/h5_chunk_read.c
h5_compound: $(srcdir)/h5_compound.c
h5_extend_write: $(srcdir)/h5_extend_write.c
h5_group: $(srcdir)/h5_group.c
h5_write: $(srcdir)/h5_write.c
h5_read: $(srcdir)/h5_read.c
h5_select: $(srcdir)/h5_select.c
h5_attribute: $(srcdir)/h5_attribute.c
h5_mount: $(srcdir)/h5_mount.c
h5_reference: $(srcdir)/h5_reference.c
h5_drivers: $(srcdir)/h5_drivers.c
ph5example: $(srcdir)/ph5example.c
h5_dtransform: $(srcdir)/h5_dtransform.c

include $(top_srcdir)/config/conclude.am
