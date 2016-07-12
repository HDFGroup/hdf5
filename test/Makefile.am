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
# HDF5 Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_builddir)/src

# Test script for error_test and err_compat
TEST_SCRIPT = testerror.sh testlibinfo.sh testcheck_version.sh testlinks_env.sh
SCRIPT_DEPEND = error_test$(EXEEXT) err_compat$(EXEEXT) links_env$(EXEEXT)
if HAVE_SHARED_CONDITIONAL
  TEST_SCRIPT += test_plugin.sh
  SCRIPT_DEPEND += plugin$(EXEEXT)
endif

check_SCRIPTS = $(TEST_SCRIPT)

# These are our main targets. They should be listed in the order to be
# executed, generally most specific tests to least specific tests.
# As an exception, long-running tests should occur earlier in the list.
# This gives them more time to run when tests are executing in parallel.
# These tests (fheap, btree2) are under development and are not used by
# the library yet. Move them to the end so that their failure do not block
# other current library code tests.
TEST_PROG=testhdf5 lheap ohdr stab gheap cache cache_api \
           pool accum hyperslab istore bittests dt_arith \
           dtypes dsets cmpd_dset filter_fail extend external efc objcopy links unlink \
           big mtime fillval mount flush1 flush2 app_ref enum \
           set_extent ttsafe \
           getname vfd ntypes dangle dtransform reserved cross_read \
           freespace mf btree2 fheap file_image unregister

# List programs to be built when testing here. error_test and err_compat are
# built at the same time as the other tests, but executed by testerror.sh.
# tcheck_version is used by testcheck_version.sh.
# 'make check' doesn't run them directly, so they are not included in TEST_PROG.
# Also build testmeta, which is used for timings test.  It builds quickly,
# and this lets automake keep all its test programs in one place.
check_PROGRAMS=$(TEST_PROG) error_test err_compat tcheck_version testmeta links_env
if HAVE_SHARED_CONDITIONAL
  check_PROGRAMS+= plugin
endif

# These programs generate test files for the tests.  They don't need to be
# compiled every time we want to test the library.  However, putting
# them in a conditional causes automake to generate rules so that they
# can be built by hand.  They can also be built by specifying
# --enable-build-all at configure time.
# The gen_old_* files can only be compiled with older versions of the library
# so do not appear in this list.
BUILD_ALL_PROGS=gen_bad_ohdr gen_bogus gen_cross gen_deflate gen_filters gen_idx \
    gen_new_array gen_new_fill gen_new_group gen_new_mtime gen_new_super \
    gen_noencoder gen_nullspace gen_udlinks space_overflow gen_sizes_lheap \
    gen_file_image

if BUILD_ALL_CONDITIONAL
  noinst_PROGRAMS=$(BUILD_ALL_PROGS)
endif

if HAVE_SHARED_CONDITIONAL
  # The libh5test library provides common support code for the tests.
  noinst_LTLIBRARIES=libh5test.la

  # The libdynlib1, libdynlib2, libdynlib3, and libdynlib4 library for testing plugin module plugin.c.
  # Build it as shared library if configure is enabled for shared library.
  lib_LTLIBRARIES=libdynlib1.la libdynlib2.la libdynlib3.la libdynlib4.la
  libdynlib1_la_SOURCES=dynlib1.c
  libdynlib2_la_SOURCES=dynlib2.c
  libdynlib3_la_SOURCES=dynlib3.c
  libdynlib4_la_SOURCES=dynlib4.c

install-exec-hook:
	$(RM) $(DESTDIR)$(libdir)/*dynlib*

else
  # The libh5test library provides common support code for the tests.
  noinst_LTLIBRARIES=libh5test.la
endif

libh5test_la_SOURCES=h5test.c testframe.c cache_common.c

# Use libhd5test.la to compile all of the tests
LDADD=libh5test.la $(LIBHDF5)

# List the source files for tests that have more than one
ttsafe_SOURCES=ttsafe.c ttsafe_dcreate.c ttsafe_error.c ttsafe_cancel.c       \
               ttsafe_acreate.c

VFD_LIST = sec2 stdio core core_paged split multi family
if DIRECT_VFD_CONDITIONAL
  VFD_LIST += direct
endif

# Additional target for running timing test
timings _timings: testmeta
	@for timing in $(TIMINGS) dummy; do                                   \
	   if test $$timing != dummy; then                                    \
	      echo "Running $$timing $(TEST_FLAGS)";                          \
	      $(RUNEXEC) ./$$timing $(TEST_FLAGS) || exit 1;                  \
	   fi;                                                                \
	done;


# The flush1 test must run before the flush2 test
flush2.chkexe_: flush1.chkexe_

# Temporary files.  These files are the ones created by setting the
# HDF5_NOCLEANUP environment variable and running `make test' without
# specifying a file prefix or low-level driver.  Changing the file
# prefix or low-level driver with environment variables will influence
# the temporary file name in ways that the makefile is not aware of.
CHECK_CLEANFILES+=accum.h5 cmpd_dset.h5 compact_dataset.h5 dataset.h5 dset_offset.h5 \
    max_compact_dataset.h5 simple.h5 set_local.h5 random_chunks.h5 \
    huge_chunks.h5 chunk_cache.h5 big_chunk.h5 chunk_expand.h5 \
    copy_dcpl_newfile.h5 extend.h5 istore.h5 extlinks*.h5 frspace.h5 links*.h5 \
    sys_file1 tfile[1-7].h5 th5s[1-4].h5 lheap.h5 fheap.h5 ohdr.h5 \
    stab.h5 extern_[1-4].h5 extern_[1-4][ab].raw gheap[0-4].h5 \
    dt_arith[1-2] links.h5 links[0-6]*.h5 extlinks[0-15].h5 tmp \
    big.data big[0-9][0-9][0-9][0-9][0-9].h5  \
    stdio.h5 sec2.h5 dtypes[0-9].h5 dtypes1[0].h5 dt_arith[1-2].h5 tattr.h5 \
    tselect.h5 mtime.h5 unlink.h5 unicode.h5 coord.h5 \
    fillval_[0-9].h5 fillval.raw mount_[0-9].h5 testmeta.h5 ttime.h5 \
    trefer[1-3].h5 tvltypes.h5 tvlstr.h5 tvlstr2.h5 flush.h5         \
    enum1.h5 titerate.h5 ttsafe.h5 tarray1.h5 tgenprop.h5            \
    tmisc[0-9]*.h5 set_extent[1-5].h5 ext[12].bin           \
    getname.h5 getname[1-3].h5 sec2_file.h5 direct_file.h5           \
    family_file000[0-3][0-9].h5 new_family_v16_000[0-3][0-9].h5      \
    multi_file-[rs].h5 core_file plugin.h5 \
    new_move_[ab].h5 ntypes.h5 dangle.h5 error_test.h5 err_compat.h5 \
    dtransform.h5 test_filters.h5 get_file_name.h5 tstint[1-2].h5    \
    unlink_chunked.h5 btree2.h5 objcopy_src.h5 objcopy_dst.h5        \
    objcopy_ext.dat trefer1.h5 trefer2.h5 app_ref.h5 tcheck_version_*.out \
    tcheck_version_*.err efc[0-5].h5 log_vfd_out.log \
    new_multi_file_v16-r.h5 new_multi_file_v16-s.h5 \
    split_get_file_image_test-m.h5 split_get_file_image_test-r.h5    \
    file_image_core_test.h5.copy unregister_filter_1.h5 unregister_filter_2.h5

# Sources for testhdf5 executable
testhdf5_SOURCES=testhdf5.c tarray.c tattr.c tchecksum.c tconfig.c tfile.c \
    tgenprop.c th5o.c th5s.c tcoords.c theap.c tid.c titerate.c tmeta.c tmisc.c \
    trefer.c trefstr.c tselect.c tskiplist.c tsohm.c ttime.c ttst.c tunicode.c \
    tvlstr.c tvltypes.c

# Temporary files.
DISTCLEANFILES=testerror.sh testlibinfo.sh testcheck_version.sh testlinks_env.sh test_plugin.sh

include $(top_srcdir)/config/conclude.am
