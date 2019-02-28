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
# HDF5 Java native interface (JNI) Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am
include $(top_srcdir)/config/lt_vers.am

# Mark this directory as part of the JNI API
JAVA_API=yes

# Include src directory and JNI flags
AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_srcdir)/java/src/jni $(JNIFLAGS)

# This is our main target
lib_LTLIBRARIES=libhdf5_java.la

# Add libtool numbers to the HDF5 Java (JNI) library (from config/lt_vers.am)
libhdf5_java_la_LDFLAGS = -avoid-version -shared -export-dynamic -version-info $(LT_JAVA_VERS_INTERFACE):$(LT_JAVA_VERS_REVISION):$(LT_JAVA_VERS_AGE) $(AM_LDFLAGS)

# Source files for the library
libhdf5_java_la_SOURCES=exceptionImp.c h5Constants.c nativeData.c h5util.c h5Imp.c \
	h5aImp.c h5dImp.c h5eImp.c h5fImp.c h5gImp.c h5iImp.c h5lImp.c h5oImp.c    \
	h5pImp.c h5pACPLImp.c h5pDAPLImp.c h5pDCPLImp.c h5pDXPLImp.c \
	h5pFAPLImp.c h5pFCPLImp.c h5pGAPLImp.c h5pGCPLImp.c h5pLAPLImp.c h5pLCPLImp.c \
	h5pOCPLImp.c h5pOCpyPLImp.c h5pStrCPLImp.c h5plImp.c h5rImp.c h5sImp.c h5tImp.c \
	h5zImp.c

# HDF5 Java (JNI) library depends on HDF5 Library.
libhdf5_java_la_LIBADD=$(LIBHDF5)

include $(top_srcdir)/config/conclude.am
