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
# HDF5 Java native interface (JNI) Library Makefile(.in)

include $(top_srcdir)/config/commence.am

if BUILD_TESTS_CONDITIONAL
   TESTSERIAL_DIR =test
   TESTEXAMPLES_DIR =examples
else
   TESTSERIAL_DIR=
   TESTEXAMPLES_DIR=
endif

## Only recurse into subdirectories if the Java (JNI) interface is enabled.
if BUILD_JAVA_CONDITIONAL

# Mark this directory as part of the JNI API
JAVA_API=yes

SUBDIRS=src $(TESTSERIAL_DIR) $(TESTEXAMPLES_DIR)

endif

include $(top_srcdir)/config/conclude.am
