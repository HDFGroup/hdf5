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
# HDF5 High-Level Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am

# Add include directories to the C preprocessor flags
AM_CPPFLAGS=-I$(top_srcdir)/src

# This library is our main target.
lib_LTLIBRARIES=libhdf5_hl.la

libhdf5_hl_la_SOURCES=H5LT.c H5TB.c H5IM.c H5DS.c H5PT.c H5HL_private.c

# Public header files (to be installed)
include_HEADERS=H5IM.h H5LT.h H5TB.h H5DS.h H5PT.h

include $(top_srcdir)/config/conclude.am
