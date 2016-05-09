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
#
# 
# This makefile mostly just reinvokes make in the various subdirectories
# but does so in the correct order.  You can alternatively invoke make from
# each subdirectory manually.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# HDF5 Java Library Examples Makefile(.in)

include $(top_srcdir)/config/commence.am

## Only recurse into subdirectories if the Java (JNI) interface is enabled.
   SUBDIRS=intro groups datasets datatypes

include $(top_srcdir)/config/conclude.am
