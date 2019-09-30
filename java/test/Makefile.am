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
# HDF5 Java native interface (JNI) Library Test Makefile(.in)

include $(top_srcdir)/config/commence.am

# Mark this directory as part of the JNI API
JAVA_API=yes

JAVAROOT = .classes

classes:
	test -d $(@D)/$(JAVAROOT) || $(MKDIR_P) $(@D)/$(JAVAROOT)

pkgpath = test
hdfjarfile = jar$(PACKAGE_TARNAME)-$(PACKAGE_VERSION).jar
CLASSPATH_ENV=CLASSPATH=.:$(JAVAROOT):$(top_builddir)/java/src/$(hdfjarfile):$(top_srcdir)/java/lib/junit.jar:$(top_srcdir)/java/lib/hamcrest-core.jar:$(top_srcdir)/java/lib/slf4j-api-1.7.25.jar:$(top_srcdir)/java/lib/ext/slf4j-simple-1.7.25.jar:$$CLASSPATH

jarfile = jar$(PACKAGE_TARNAME)test.jar

AM_JAVACFLAGS = $(H5_JAVACFLAGS) -deprecation

TESTPACKAGE = 

noinst_JAVA = \
	TestH5.java \
    TestH5Eparams.java \
    TestH5Eregister.java \
    TestH5Fparams.java \
    TestH5Fbasic.java \
    TestH5F.java \
    TestH5Fswmr.java \
    TestH5Gbasic.java \
    TestH5G.java \
    TestH5Sbasic.java \
    TestH5S.java \
    TestH5Tparams.java \
    TestH5Tbasic.java \
    TestH5T.java \
    TestH5Dparams.java \
    TestH5D.java \
    TestH5Dplist.java \
    TestH5Lparams.java \
    TestH5Lbasic.java \
    TestH5Lcreate.java \
    TestH5R.java \
    TestH5P.java \
    TestH5PData.java \
    TestH5Pfapl.java \
    TestH5Pfaplhdfs.java \
    TestH5Pfapls3.java \
    TestH5Pvirtual.java \
    TestH5Plist.java \
    TestH5A.java \
    TestH5Oparams.java \
    TestH5Obasic.java \
    TestH5Ocreate.java \
    TestH5Ocopy.java \
    TestH5PL.java \
    TestH5Z.java \
    TestH5E.java \
    TestH5Edefault.java \
    TestH5Giterate.java

$(jarfile): classnoinst.stamp classes
	$(JAR) cvf $@ -C $(JAVAROOT)/ $(pkgpath)

noinst_DATA = $(jarfile)

.PHONY: classes

check_SCRIPTS = junit.sh
TEST_SCRIPT = $(check_SCRIPTS)

CLEANFILES = classnoinst.stamp $(jarfile) $(JAVAROOT)/$(pkgpath)/*.class junit.sh

clean:
	rm -rf $(JAVAROOT)/*
	rm -f $(jarfile)
	rm -f classnoinst.stamp

include $(top_srcdir)/config/conclude.am
