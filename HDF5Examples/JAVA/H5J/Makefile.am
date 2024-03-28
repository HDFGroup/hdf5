#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#

JAVAROOT = .classes

classes:
  $(MKDIR_P) $(@D)/$(JAVAROOT)

pkgpath = examples/intro
hdfjarfile = jar$(PACKAGE_TARNAME)-$(PACKAGE_VERSION).jar
CLASSPATH_ENV=CLASSPATH=.:$(JAVAROOT):$(top_builddir)/java/src/$(hdfjarfile):$$CLASSPATH

jarfile = jar$(PACKAGE_TARNAME)intro.jar

AM_JAVACFLAGS = $(H5_JAVACFLAGS) -deprecation

TESTPACKAGE =

noinst_JAVA = \
    HDF5AttributeCreate.java \
    HDF5DatasetCreate.java \
    HDF5DatasetRead.java \
    HDF5FileCreate.java \
    HDF5FileStructure.java \
    HDF5GroupCreate.java \
    HDF5SubsetSelect.java

$(jarfile): classnoinst.stamp classes
  $(JAR) cvf $@ -C $(JAVAROOT)/ $(pkgpath)

noinst_DATA = $(jarfile)

TESTS = runExample.sh

CLEANFILES = classnoinst.stamp $(jarfile) $(JAVAROOT)/$(pkgpath)/*.class runExample.sh

clean:
  rm -rf $(JAVAROOT)
  rm -f $(jarfile)
  rm -f classnoinst.stamp
