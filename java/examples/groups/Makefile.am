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
##
#
# HDF5 Java Library Examples Makefile(.in)

include $(top_srcdir)/config/commence.am

# Mark this directory as part of the JNI API
JAVA_API=yes

JAVAROOT = .classes

classes:
	test -d $(@D)/$(JAVAROOT) || $(MKDIR_P) $(@D)/$(JAVAROOT)

pkgpath = examples/groups
hdfjarfile = jar$(PACKAGE_TARNAME)-$(PACKAGE_VERSION).jar
CLASSPATH_ENV=CLASSPATH=.:$(JAVAROOT):$(top_builddir)/java/src/$(hdfjarfile):$(top_srcdir)/java/lib/slf4j-api-1.7.5.jar:$(top_srcdir)/java/lib/ext/slf4j-simple-1.7.5.jar:$$CLASSPATH

jarfile = jar$(PACKAGE_TARNAME)groups.jar

AM_JAVACFLAGS = $(H5_JAVACFLAGS) -deprecation

TESTPACKAGE = 

noinst_JAVA = \
    H5Ex_G_Create.java \
    H5Ex_G_Iterate.java \
    H5Ex_G_Compact.java \
    H5Ex_G_Corder.java \
    H5Ex_G_Intermediate.java \
    H5Ex_G_Phase.java \
    H5Ex_G_Visit.java


$(jarfile): classnoinst.stamp classes
	$(JAR) cvf $@ -C $(JAVAROOT)/ $(pkgpath)

noinst_DATA = $(jarfile)

.PHONY: classes

check_SCRIPTS = runExample.sh
TEST_SCRIPT = $(check_SCRIPTS)

CLEANFILES = classnoinst.stamp $(jarfile) $(JAVAROOT)/$(pkgpath)/*.class runExample.sh

clean:
	rm -rf $(JAVAROOT)/*
	rm -f $(jarfile)
	rm -f classnoinst.stamp

include $(top_srcdir)/config/conclude.am
