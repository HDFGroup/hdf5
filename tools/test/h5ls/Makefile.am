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
  lib_LTLIBRARIES=libdynlibls.la
  libdynlibls_la_SOURCES=dynlib_ls.c

install-exec-hook:
	$(RM) $(DESTDIR)$(libdir)/*dynlib*
endif

DISTCLEANFILES=h5ls_plugin.sh

include $(top_srcdir)/config/conclude.am
