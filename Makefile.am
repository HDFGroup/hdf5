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
# This makefile mostly just reinvokes make in the various subdirectories
# but does so in the correct order.  You can alternatively invoke make from
# each subdirectory manually.
#
# Top-level HDF5 Makefile(.in)

include $(top_srcdir)/config/commence.am

# Define subdirectories to build.
## Automake understands that `make distclean' should recurse into
## conditional subdirectories even if `make all' does not.
## We need to list examples and doc in the DIST_SUBDIRS variable
## so that they also will be visited by `make distclean'
# Add this directory to SUBDIRS so that examples get built after tools
# but before examples in extra interfaces (c++ and fortran).
# Since we're explicitly listing DIST_SUBDIRS, we also need to list
# directories that are only conditionally built (so that their Makefiles
# are cleaned as well).
# Note that `make clean' will not affect the examples or doc directories. 

if BUILD_PARALLEL_CONDITIONAL
   TESTPARALLEL_DIR =testpar
endif
if BUILD_CXX_CONDITIONAL
   CXX_DIR =c++
endif
if BUILD_FORTRAN_CONDITIONAL
   FORTRAN_DIR =fortran
endif
if BUILD_HDF5_HL_CONDITIONAL
   HDF5_HL_DIR =hl
endif

SUBDIRS = src test $(TESTPARALLEL_DIR) tools . $(CXX_DIR) $(FORTRAN_DIR) \
         $(HDF5_HL_DIR)
DIST_SUBDIRS = src test testpar tools . c++ fortran hl perform examples  \
          doc 

# Some files generated during configure that should be cleaned
DISTCLEANFILES=src/H5config.h src/H5pubconf.h config/stamp1 config/stamp2

# Define rules for lib, progs, check, and tests.
# These simply involve recursing into subdirectories.
test _test: check

lib progs:
	@@SETX@; for d in $(SUBDIRS); do                                \
          if test $$d != .; then                                        \
	   (cd $$d && $(MAKE) $@) || exit 1;                            \
          fi;                                                           \
	done

# Make all and make tests also create tests in perform directory
all-local:
	cd perform && $(MAKE) all
tests:
	@@SETX@; for d in $(SUBDIRS) perform; do                        \
          if test $$d != .; then                                        \
	   (cd $$d && $(MAKE) $@) || exit 1;                            \
          fi;                                                           \
	done

# Clean and mostlyclean need to recurse into docs, examples, and perform
# directories unless they've already been distcleaned.
clean-local:
	@@SETX@; for d in docs examples perform; do                     \
	  if test -f $$d/Makefile; then                                 \
	    (cd $$d && $(MAKE) clean) || exit 1;                        \
	  fi;                                                           \
	done

mostlyclean-local:
	@@SETX@; for d in docs examples perform; do                     \
	  if test -f $$d/Makefile; then                                 \
	    (cd $$d && $(MAKE) mostlyclean) || exit 1;                  \
	  fi;                                                           \
	done

# 'make install-all' also installs docs and examples
install-all:
	$(MAKE) install
	$(MAKE) install-doc
uninstall-all:
	$(MAKE) uninstall
	$(MAKE) uninstall-doc

# Install examples in this directory and recursively
install-examples uninstall-examples:
	@@SETX@; for d in examples $(HDF5_INTERFACES); do               \
	    (cd $$d && $(MAKE) $@) || exit 1;                           \
	done

# Install documentation and examples
install-doc:
	$(MAKE) install-examples
	(cd doc && $(MAKE) $@) || exit 1;

uninstall-doc:
	$(MAKE) uninstall-examples
	(cd doc && $(MAKE) $@) || exit 1;

# `make check-install' or `make installcheck' checks that examples can 
#  be successfully built
check-install:
	$(MAKE) installcheck

installcheck-local:
	(cd examples && $(MAKE) $@) || exit 1;

# Don't include conclude.am in root Makefile; tests target needs to
# recurse into perform directory as well as reguar subdirs.
