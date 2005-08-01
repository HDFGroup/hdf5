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
#
# This makefile mostly just reinvokes make in the various subdirectories
# but does so in the correct order.  You can alternatively invoke make from
# each subdirectory manually.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# Top-level HDF5-Fortran Makefile(.in)
#

include $(top_srcdir)/config/commence.am

if BUILD_PARALLEL_CONDITIONAL
  TESTPARALLEL_DIR=testpar
endif

# Subdirectories in build order, not including examples directory
## Only recurse into subdirectories if HDF5 is configured to use Fortran.
if BUILD_FORTRAN_CONDITIONAL
   SUBDIRS=src test $(TESTPARALLEL_DIR)
endif

# All directories that have Makefiles
DIST_SUBDIRS=src test testpar examples

# Clean and mostlyclean need to recurse into examples directory     
clean-local:     
	@if test -f examples/Makefile; then                                \
	  (cd examples && $(MAKE) $(AM_MAKEFLAGS) clean) || exit 1;       \
	fi;
mostlyclean-local:         
	@if test -f examples/Makefile; then                                \
	  (cd examples && $(MAKE) $(AM_MAKEFLAGS) mostlyclean) || exit 1; \
	fi; 

# Install examples
install-examples uninstall-examples:
	@(cd examples && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;


# Check that examples can be successfully built 
check-install:
	@$(MAKE) $(AM_MAKEFLAGS) installcheck 

installcheck-local:
	@(cd examples && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;

include $(top_srcdir)/config/conclude.am
