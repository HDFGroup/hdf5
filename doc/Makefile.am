# HDF5 Library Doc Makefile(.in)
#
# Copyright (C) 1997, 2002
#     National Center for Supercomputing Applications.
#     All rights reserved.
#
# 
# This is the top level makefile of the Doc directory.  It mostly just
# reinvokes make in the various subdirectories.
# You can alternatively invoke make from each subdirectory manually.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#

# Add doc-specific include
include $(top_srcdir)/config/commence-doc.am

# Subdirectories in build-order
SUBDIRS=html

# Don't include conclude, since the docs directory doesn't need to know how to
# build anything.
