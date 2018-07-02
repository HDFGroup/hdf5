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
##
#
# HDF5 Java Library Examples Makefile(.in)

include $(top_srcdir)/config/commence.am

# Mark this directory as part of the JNI API
JAVA_API=yes

JAVAROOT = .classes

classes:
	test -d $(@D)/$(JAVAROOT) || $(MKDIR_P) $(@D)/$(JAVAROOT)

pkgpath = examples/datasets
hdfjarfile = jar$(PACKAGE_TARNAME)-$(PACKAGE_VERSION).jar
CLASSPATH_ENV=CLASSPATH=.:$(JAVAROOT):$(top_builddir)/java/src/$(hdfjarfile):$(top_srcdir)/java/lib/slf4j-api-1.7.25.jar:$(top_srcdir)/java/lib/ext/slf4j-simple-1.7.25.jar:$$CLASSPATH

jarfile = jar$(PACKAGE_TARNAME)datasets.jar

AM_JAVACFLAGS = $(H5_JAVACFLAGS) -deprecation

TESTPACKAGE = 

noinst_JAVA = \
	H5Ex_D_Alloc.java \
    H5Ex_D_Checksum.java \
    H5Ex_D_Chunk.java \
    H5Ex_D_Compact.java \
    H5Ex_D_External.java \
    H5Ex_D_FillValue.java \
    H5Ex_D_Gzip.java \
    H5Ex_D_Hyperslab.java \
    H5Ex_D_ReadWrite.java \
    H5Ex_D_Shuffle.java \
    H5Ex_D_Szip.java \
    H5Ex_D_UnlimitedAdd.java \
    H5Ex_D_UnlimitedGzip.java \
    H5Ex_D_UnlimitedMod.java \
    H5Ex_D_Nbit.java \
    H5Ex_D_Transform.java \
    H5Ex_D_Sofloat.java \
    H5Ex_D_Soint.java

$(jarfile): classnoinst.stamp classes
	$(JAR) cvf $@ -C $(JAVAROOT)/ $(pkgpath)

noinst_DATA = $(jarfile)

.PHONY: classes

check_SCRIPTS = JavaDatasetExample.sh
TEST_SCRIPT = $(check_SCRIPTS)

CLEANFILES = classnoinst.stamp $(jarfile) $(JAVAROOT)/$(pkgpath)/*.class JavaDatasetExample.sh

clean:
	rm -rf $(JAVAROOT)/*
	rm -f $(jarfile)
	rm -f classnoinst.stamp

include $(top_srcdir)/config/conclude.am
