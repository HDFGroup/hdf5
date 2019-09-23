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

# Mark this directory as part of the JNI API
JAVA_API=yes

SUBDIRS=jni

JAVAROOT = .classes

classes:
	test -d $(@D)/$(JAVAROOT) || $(MKDIR_P) $(@D)/$(JAVAROOT)

jarfile = jar$(PACKAGE_TARNAME)-$(PACKAGE_VERSION).jar
hdf5_javadir = $(libdir)

pkgpath = hdf/hdf5lib
CLASSPATH_ENV=CLASSPATH=.:$(top_srcdir)/java/lib/slf4j-api-1.7.25.jar:$$CLASSPATH

AM_JAVACFLAGS = $(H5_JAVACFLAGS) -deprecation

hdf5_java_JAVA =  \
	${pkgpath}/callbacks/Callbacks.java \
	${pkgpath}/callbacks/H5A_iterate_cb.java \
	${pkgpath}/callbacks/H5A_iterate_t.java \
	${pkgpath}/callbacks/H5D_append_cb.java \
	${pkgpath}/callbacks/H5D_append_t.java \
	${pkgpath}/callbacks/H5D_iterate_cb.java \
	${pkgpath}/callbacks/H5D_iterate_t.java \
	${pkgpath}/callbacks/H5E_walk_cb.java \
	${pkgpath}/callbacks/H5E_walk_t.java \
	${pkgpath}/callbacks/H5L_iterate_cb.java \
	${pkgpath}/callbacks/H5L_iterate_t.java \
	${pkgpath}/callbacks/H5O_iterate_cb.java \
	${pkgpath}/callbacks/H5O_iterate_t.java \
    ${pkgpath}/callbacks/H5P_cls_close_func_cb.java \
    ${pkgpath}/callbacks/H5P_cls_close_func_t.java \
    ${pkgpath}/callbacks/H5P_cls_copy_func_cb.java \
    ${pkgpath}/callbacks/H5P_cls_copy_func_t.java \
    ${pkgpath}/callbacks/H5P_cls_create_func_cb.java \
    ${pkgpath}/callbacks/H5P_cls_create_func_t.java \
    ${pkgpath}/callbacks/H5P_prp_close_func_cb.java \
    ${pkgpath}/callbacks/H5P_prp_compare_func_cb.java \
    ${pkgpath}/callbacks/H5P_prp_copy_func_cb.java \
    ${pkgpath}/callbacks/H5P_prp_create_func_cb.java \
    ${pkgpath}/callbacks/H5P_prp_delete_func_cb.java \
    ${pkgpath}/callbacks/H5P_prp_get_func_cb.java \
    ${pkgpath}/callbacks/H5P_prp_set_func_cb.java \
 	${pkgpath}/callbacks/H5P_iterate_cb.java \
	${pkgpath}/callbacks/H5P_iterate_t.java \
	${pkgpath}/exceptions/HDF5AtomException.java  \
	${pkgpath}/exceptions/HDF5AttributeException.java  \
	${pkgpath}/exceptions/HDF5BtreeException.java  \
	${pkgpath}/exceptions/HDF5DataFiltersException.java  \
	${pkgpath}/exceptions/HDF5DataStorageException.java  \
	${pkgpath}/exceptions/HDF5DatasetInterfaceException.java  \
	${pkgpath}/exceptions/HDF5DataspaceInterfaceException.java  \
	${pkgpath}/exceptions/HDF5DatatypeInterfaceException.java  \
	${pkgpath}/exceptions/HDF5Exception.java  \
	${pkgpath}/exceptions/HDF5ExternalFileListException.java  \
	${pkgpath}/exceptions/HDF5FileInterfaceException.java  \
	${pkgpath}/exceptions/HDF5FunctionArgumentException.java  \
	${pkgpath}/exceptions/HDF5FunctionEntryExitException.java  \
	${pkgpath}/exceptions/HDF5HeapException.java  \
	${pkgpath}/exceptions/HDF5InternalErrorException.java  \
	${pkgpath}/exceptions/HDF5JavaException.java  \
	${pkgpath}/exceptions/HDF5LibraryException.java  \
	${pkgpath}/exceptions/HDF5LowLevelIOException.java  \
	${pkgpath}/exceptions/HDF5MetaDataCacheException.java  \
	${pkgpath}/exceptions/HDF5ObjectHeaderException.java  \
	${pkgpath}/exceptions/HDF5PropertyListInterfaceException.java  \
	${pkgpath}/exceptions/HDF5ReferenceException.java  \
	${pkgpath}/exceptions/HDF5ResourceUnavailableException.java  \
	${pkgpath}/exceptions/HDF5SymbolTableException.java \
	${pkgpath}/structs/H5_ih_info_t.java \
	${pkgpath}/structs/H5A_info_t.java \
	${pkgpath}/structs/H5E_error2_t.java \
	${pkgpath}/structs/H5F_info2_t.java \
	${pkgpath}/structs/H5FD_hdfs_fapl_t.java \
	${pkgpath}/structs/H5FD_ros3_fapl_t.java \
	${pkgpath}/structs/H5G_info_t.java \
	${pkgpath}/structs/H5L_info_t.java \
	${pkgpath}/structs/H5O_info_t.java \
	${pkgpath}/structs/H5O_hdr_info_t.java \
	${pkgpath}/structs/H5AC_cache_config_t.java \
	${pkgpath}/H5.java  \
	${pkgpath}/HDF5Constants.java  \
	${pkgpath}/HDF5GroupInfo.java  \
	${pkgpath}/HDFArray.java  \
	${pkgpath}/HDFNativeData.java

$(jarfile): classhdf5_java.stamp classes docs
	$(JAR) cvf $@ -C $(JAVAROOT)/ $(pkgpath)

hdf5_java_DATA = $(jarfile)

.PHONY: docs classes

WINDOWTITLE = 'HDF5 Java'
DOCTITLE = '<h1>HDF5 Java Wrapper</h1>'
SRCDIR = '$(pkgpath)'

docs:
	$(JAVADOC) -sourcepath $(srcdir) -d javadoc -use -splitIndex -windowtitle $(WINDOWTITLE) -doctitle $(DOCTITLE) -J-Xmx180m -verbose -overview $(top_srcdir)/java/src/hdf/overview.html -classpath $(CLASSPATH_ENV) hdf.hdf5lib

CLEANFILES = classhdf5_java.stamp $(jarfile) $(JAVAROOT)/$(pkgpath)/callbacks/*.class $(JAVAROOT)/$(pkgpath)/exceptions/*.class $(JAVAROOT)/$(pkgpath)/structs/*.class $(JAVAROOT)/$(pkgpath)/*.class

clean:
	rm -rf $(JAVAROOT)/*
	rm -f $(jarfile)
	rm -rf javadoc
	rm -f classhdf5_java.stamp


# Clean examples when check-clean is invoked
check-clean :: ;

#include $(top_srcdir)/config/conclude.am
