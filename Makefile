# Top-level distributed Makefile 			       -*- makefile -*-

# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

# This Makefile is a stub (copied from Makefile.dist) which will run
# configure and then invoke the same target in the new Makefile created
# by configure.

# Uncomment this variable if your make(1) doesn't set it automatically.
#
#MAKE=make


SHELL=/bin/sh

all lib progs check test _test install uninstall dep depend: _config
	$(MAKE) $@

clean mostlyclean distclean maintainer-clean TAGS: _config
	$(MAKE) $@

_config:
	sh configure

.PHONY: all lib progs test install uninstall dep depend clean mostlyclean     \
	distclean maintainer-clean _config
