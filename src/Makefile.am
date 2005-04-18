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
# HDF5 Library Makefile(.in)
#

include $(top_srcdir)/config/commence.am
include $(top_srcdir)/config/lt_vers.am

# How to build H5detect for number format detection.
# Use -g to force no optimization since many compilers (e.g., Intel) takes
# a long time to compile it with any optimization on.  H5detect is used
# to generate H5Tinit.c once. So, optimization is not critical.
noinst_PROGRAMS = H5detect
H5detect_CFLAGS = -g

# Our main target, the HDF5 library
lib_LTLIBRARIES=libhdf5.la

# Add libtool numbers to the HDF5 library (from config/lt_vers.am)
libhdf5_la_LDFLAGS= -version-info $(LT_VERS_INTERFACE):$(LT_VERS_REVISION):$(LT_VERS_AGE)

# Temporary files
MOSTLYCLEANFILES=H5detect.o H5detect.lo H5detect H5Tinit.o H5Tinit.lo H5Tinit.c

# libhdf5.settings is generated during configure. Remove it when distclean.
DISTCLEAN=libhdf5.settings


# library sources
libhdf5_la_SOURCES= H5.c H5A.c H5AC.c H5B.c H5B2.c H5B2cache.c H5B2dbg.c      \
        H5B2test.c H5BT.c H5BTbtree2.c H5BTcache.c H5BTdbg.c H5BTtest.c H5C.c \
        H5D.c \
        H5Dcontig.c \
        H5Dcompact.c \
        H5Defl.c H5Dio.c H5Distore.c H5Dmpio.c H5Dselect.c H5Dtest.c H5E.c H5F.c \
        H5Fdbg.c H5FD.c H5FDcore.c  \
        H5FDfamily.c H5FDfphdf5.c H5FDlog.c H5FDmpi.c H5FDmpio.c   \
        H5FDmpiposix.c H5FDmulti.c H5FDsec2.c H5FDstdio.c           \
        H5FDstream.c H5FL.c H5FO.c H5FP.c H5FPclient.c H5FPserver.c H5FS.c    \
        H5G.c H5Gent.c H5Gnode.c H5Gstab.c  \
        H5HG.c H5HGdbg.c H5HL.c H5HLdbg.c H5HP.c H5I.c H5MF.c H5MM.c H5O.c    \
        H5Oattr.c H5Obogus.c   \
        H5Ocont.c H5Odtype.c H5Oefl.c H5Ofill.c H5Olayout.c H5Omtime.c        \
        H5Oname.c H5Onull.c H5Opline.c H5Osdspace.c H5Oshared.c H5Ostab.c     \
        H5P.c H5Pdcpl.c H5Pdxpl.c H5Pfapl.c H5Pfcpl.c H5Ptest.c H5R.c H5RC.c  \
        H5RS.c H5S.c H5Sall.c H5Shyper.c H5Smpio.c H5Snone.c H5Spoint.c       \
        H5Sselect.c H5Stest.c H5SH.c H5SHcache.c H5SHdbg.c \
        H5SL.c H5ST.c H5T.c H5Tarray.c H5Tbit.c H5Tcommit.c \
        H5Tcompound.c H5Tconv.c H5Tcset.c H5Tenum.c H5Tfields.c H5Tfixed.c    \
        H5Tfloat.c H5Tinit.c H5Tnative.c H5Toffset.c H5Topaque.c H5Torder.c   \
        H5Tpad.c H5Tprecis.c H5Tstrpad.c H5Tvlen.c H5TS.c H5V.c H5Z.c  \
        H5Zdeflate.c H5Zfletcher32.c H5Znbit.c H5Zshuffle.c H5Zszip.c  \
        H5Zscaleoffset.c H5Ztrans.c


# Public headers
include_HEADERS =H5public.h H5Apublic.h H5ACpublic.h H5Bpublic.h H5B2public.h \
        H5BTpublic.h H5Cpublic.h H5Dpublic.h  \
        H5Epubgen.h H5Epublic.h H5Fpublic.h H5FDpublic.h H5FDcore.h H5FDfamily.h          \
        H5FDfphdf5.h H5FDlog.h H5FDmpi.h H5FDmpio.h H5FDmpiposix.h              \
        H5FDmulti.h H5FDsec2.h  H5FDstdio.h H5FDstream.h H5FPpublic.h             \
        H5Gpublic.h H5HGpublic.h H5HLpublic.h H5Ipublic.h                     \
        H5MMpublic.h H5Opublic.h H5Ppublic.h H5Rpublic.h H5Spublic.h \
        H5SHpublic.h \
        H5Tpublic.h H5Zpublic.h H5pubconf.h hdf5.h H5api_adpt.h

# Private headers
#noinst_HEADERS =H5private.h H5Aprivate.h H5Apkg.h H5ACprivate.h H5Bprivate.h  \
#        H5B2private.h \
#        H5BTprivate.h \
#        H5Dprivate.h H5Eprivate.h H5Fprivate.h H5FDprivate.h H5FLprivate.h    \
#        H5FOprivate.h H5FSprivate.h H5Gprivate.h H5Gpkg.h                     \
#        H5HGprivate.h H5HLprivate.h H5HPprivate.h H5Iprivate.h H5MFprivate.h  \
#        H5MMprivate.h H5Oprivate.h H5Opkg.h H5Pprivate.h H5Ppkg.h             \
#        H5Rprivate.h H5RCprivate.h H5RSprivate.h H5Sprivate.h H5SHprivate.h \
#        H5STprivate.h \
#        H5Tprivate.h H5Tpkg.h H5TSprivate.h H5Vprivate.h        \
#        H5Zprivate.h H5Zpkg.h H5config.h

# install libhdf5.settings in lib directory
settingsdir=$(libdir)
settings_DATA=libhdf5.settings

# Error header generation
#
# Actually, H5Einit.h, H5Eterm.h, H5Edefin.h and H5Epubgen.h all
# depend on H5err.txt, but the perl script generates them all, so just
# list one here.
$(top_srcdir)/src/H5Edefin.h: $(top_srcdir)/src/H5err.txt
	perl $(top_srcdir)/bin/make_err $>

# Number format detection
# The LD_LIBRARY_PATH setting is a klutch.
# Things should have been all set during H5detect making.
# Remove the generated .c file if errors occur unless HDF5_Make_Ignore
# is set to ignore the error.
H5Tinit.c: H5detect
	LD_LIBRARY_PATH="$$LD_LIBRARY_PATH`echo $(LDFLAGS) |                  \
		sed -e 's/-L/:/g' -e 's/ //g'`"                               \
	$(RUNSERIAL) ./H5detect > H5Tinit.c  ||                               \
	    (test $$HDF5_Make_Ignore && echo "*** Error ignored") ||          \
	    ($(RM) $@ ; exit 1)

include $(top_srcdir)/config/conclude.am
