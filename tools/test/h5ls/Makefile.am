#
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
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Include src and tools/lib directories
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/tools/lib

# Test programs and scripts
TEST_SCRIPT=testh5ls.sh testh5lsvds.sh
check_SCRIPTS=$(TEST_SCRIPT)
SCRIPT_DEPEND=../../src/h5ls/h5ls$(EXEEXT)
if HAVE_SHARED_CONDITIONAL
if USE_PLUGINS_CONDITIONAL
   TEST_SCRIPT += h5ls_plugin.sh
endif
endif

# All programs depend on the hdf5 and h5tools libraries
LDADD=$(LIBH5TOOLS) $(LIBHDF5)

if HAVE_SHARED_CONDITIONAL
  # Build it as shared library if configure is enabled for shared library.
  noinst_LTLIBRARIES=libdynlibls.la
  libdynlibls_la_SOURCES=dynlib_ls.c
  libdynlibls_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere
endif


DISTCLEANFILES=h5ls_plugin.sh

include $(top_srcdir)/config/conclude.am
