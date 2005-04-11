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
#
# Top-level HDF5-C++ Makefile(.in)
# 

include $(top_srcdir)/config/commence.am

## Only recurse into subdirectories if C++ interface is enabled.
if BUILD_CXX_CONDITIONAL
   SUBDIRS=src test
endif
DIST_SUBDIRS = $(SUBDIRS) examples

# Clean and mostlyclean need to recurse into examples directory
clean-local:
	if test -f examples/Makefile; then                                \
	  (cd examples && $(MAKE) $(AM_MAKEFLAGS) clean) || exit 1;       \
	fi;
mostlyclean-local:
	if test -f examples/Makefile; then                                \
	  (cd examples && $(MAKE) $(AM_MAKEFLAGS) mostlyclean) || exit 1; \
	fi;

# Install examples
install-examples uninstall-examples:
	(cd examples && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;

# Check that examples can be successfully built 
check-install:
	$(MAKE) $(AM_MAKEFLAGS) installcheck 

installcheck-local:
	(cd examples && $(MAKE) $(AM_MAKEFLAGS) $@) || exit 1;

include $(top_srcdir)/config/conclude.am
