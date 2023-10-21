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
# HDF5 Library Test Makefile(.in)
#

include $(top_srcdir)/config/commence.am

AM_CPPFLAGS+=-I$(top_srcdir)/src -I$(top_builddir)/src

# Test scripts--
#       test_abort_fail.sh: filenotclosed.c and del_many_dense_attrs.c
#       test_check_version.sh: tcheck_version
#       test_error.sh: err_compat, error_test
#       test_external_env.sh: external_env
#       test_flush_refresh.sh: flushrefresh
#       test_libinfo.sh:
#       test_links_env.sh: links_env
#       test_mirror.sh: mirror_vfd ../utils/mirror_vfd/*
#       test_plugin.sh: filter_plugin.c vfd_plugin.c vol_plugin.c
#       test_swmr.sh: swmr*
#       test_vds_env.sh: vds_env
#       test_vds_swmr.sh: vds_swmr*
#       test_use_cases.sh: use_append_chunk, use_append_mchunks, use_disable_mdc_flushes
TEST_SCRIPT = test_abort_fail.sh test_check_version.sh test_error.sh \
    test_flush_refresh.sh test_external_env.sh test_libinfo.sh \
    test_links_env.sh test_swmr.sh test_vds_env.sh test_vds_swmr.sh \
    test_use_cases.sh
SCRIPT_DEPEND = error_test$(EXEEXT) err_compat$(EXEEXT) links_env$(EXEEXT) \
    external_env$(EXEEXT) filenotclosed$(EXEEXT) del_many_dense_attrs$(EXEEXT) \
    flushrefresh$(EXEEXT) use_append_chunk$(EXEEXT) use_append_mchunks$(EXEEXT) use_disable_mdc_flushes$(EXEEXT) \
    swmr_generator$(EXEEXT) swmr_reader$(EXEEXT) swmr_writer$(EXEEXT) \
    swmr_remove_reader$(EXEEXT) swmr_remove_writer$(EXEEXT) swmr_addrem_writer$(EXEEXT) \
    swmr_sparse_reader$(EXEEXT) swmr_sparse_writer$(EXEEXT) swmr_start_write$(EXEEXT) \
    vds_env$(EXEEXT) vds_swmr_gen$(EXEEXT) vds_swmr_reader$(EXEEXT) vds_swmr_writer$(EXEEXT)
if HAVE_SHARED_CONDITIONAL
  TEST_SCRIPT += test_plugin.sh
  SCRIPT_DEPEND += filter_plugin$(EXEEXT) vfd_plugin $(EXEEXT) vol_plugin$(EXEEXT)
endif
if MIRROR_VFD_CONDITIONAL
  TEST_SCRIPT += test_mirror.sh
endif

check_SCRIPTS = $(TEST_SCRIPT)

# These are our main targets. They should be listed in the order to be
# executed, generally most specific tests to least specific tests.
# As an exception, long-running tests should occur earlier in the list.
# This gives them more time to run when tests are executing in parallel.
TEST_PROG= testhdf5 \
           cache cache_api cache_image cache_tagging lheap ohdr \
           stab gheap evict_on_close farray earray btree2 fheap \
           accum hyperslab istore bittests dt_arith page_buffer \
           dtypes dsets chunk_info cmpd_dset mdset cmpd_dtransform filter_fail extend direct_chunk \
           external efc objcopy objcopy_ref links unlink twriteorder big mtime \
           fillval mount select_io_dset\
           flush1 flush2 app_ref enum set_extent ttsafe enc_dec_plist \
           enc_dec_plist_cross_platform getname vfd ros3 s3comms hdfs ntypes \
           dangle dtransform reserved cross_read freespace mf vds file_image \
           unregister cache_logging cork swmr thread_id vol timer event_set \
		   onion

# List programs to be built when testing here
#
# error_test and err_compat are built at the same time as the other tests, but executed by test_error.sh
# tcheck_version is used by test_check_version.sh
# accum_swmr_reader is used by accum.c
# atomic_writer and atomic_reader are stand-alone programs
# links_env is used by test_links_env.sh
# external_env is used by test_external_env.sh
# filenotclosed and del_many_dense_attrs are used by test_abort_fail.sh
# flushrefresh is used by test_flush_refresh.sh
# use_append_chunk, use_append_mchunks and use_disable_mdc_flushes are used by test_use_cases.sh
# swmr_* files (besides swmr.c) are used by test_swmr.sh
# vds_swmr_* files are used by test_vds_swmr.sh
# vds_env is used by test_vds_env.sh
# mirror_vfd is used by test_mirror.sh
#
# 'make check' doesn't run them directly, so they are not included in TEST_PROG.
#
# Also build testmeta, which is used for the timing test. It builds quickly
# and this lets automake keep all its test programs in one place.
check_PROGRAMS=$(TEST_PROG) error_test err_compat tcheck_version \
    testmeta accum_swmr_reader atomic_writer atomic_reader external_env \
    links_env filenotclosed del_many_dense_attrs flushrefresh \
    use_append_chunk use_append_chunk_mirror use_append_mchunks use_disable_mdc_flushes \
    swmr_generator swmr_start_write swmr_reader swmr_writer swmr_remove_reader \
    swmr_remove_writer swmr_addrem_writer swmr_sparse_reader swmr_sparse_writer \
    vds_env vds_swmr_gen vds_swmr_reader vds_swmr_writer
if HAVE_SHARED_CONDITIONAL
  check_PROGRAMS+= filter_plugin vfd_plugin vol_plugin
endif
if MIRROR_VFD_CONDITIONAL
  check_PROGRAMS+= mirror_vfd
endif

# These programs generate test files for the tests.  They don't need to be
# compiled every time we want to test the library.  However, putting
# them in a conditional causes automake to generate rules so that they
# can be built by hand.  They can also be built by specifying
# --enable-build-all at configure time.
# The gen_old_* files can only be compiled with older versions of the library
# so do not appear in this list.
BUILD_ALL_PROGS=gen_bad_ohdr gen_bogus gen_cross gen_deflate gen_filters gen_new_array \
    gen_new_fill gen_new_group gen_new_mtime gen_new_super gen_noencoder \
    gen_nullspace gen_udlinks space_overflow gen_filespace gen_specmetaread \
    gen_sizes_lheap gen_file_image gen_plist gen_bad_offset gen_bounds

if BUILD_ALL_CONDITIONAL
  noinst_PROGRAMS=$(BUILD_ALL_PROGS)
endif

if HAVE_SHARED_CONDITIONAL
  # The libh5test library provides common support code for the tests.
  # The filter_plugin* libraries are for use in filter_plugin.c.
  # Build them as shared libraries if that option was enabled in configure.
  noinst_LTLIBRARIES=libh5test.la libfilter_plugin1_dsets.la libfilter_plugin2_dsets.la libfilter_plugin3_dsets.la libfilter_plugin4_groups.la libnull_vfd_plugin.la libnull_vol_connector.la
  libfilter_plugin1_dsets_la_SOURCES=filter_plugin1_dsets.c
  libfilter_plugin2_dsets_la_SOURCES=filter_plugin2_dsets.c
  libfilter_plugin3_dsets_la_SOURCES=filter_plugin3_dsets.c
  libfilter_plugin4_groups_la_SOURCES=filter_plugin4_groups.c
  libfilter_plugin1_dsets_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere
  libfilter_plugin2_dsets_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere
  libfilter_plugin3_dsets_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere
  libfilter_plugin4_groups_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere
  libfilter_plugin3_dsets_la_LIBADD=$(LIBHDF5)
  libfilter_plugin4_groups_la_LIBADD=$(LIBHDF5)

  # VFD plugin test libraries
  #
  # null_vfd_plugin is used for testing basic VFD plugin functionality.
  libnull_vfd_plugin_la_SOURCES=null_vfd_plugin.c
  libnull_vfd_plugin_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere

  # VOL plugin test libraries
  #
  # null_vol_connector is used for testing basic VOL plugin functionality.
  libnull_vol_connector_la_SOURCES=null_vol_connector.c
  libnull_vol_connector_la_LDFLAGS=$(AM_LDFLAGS) -avoid-version -module -shared -export-dynamic -rpath /nowhere

else
  # The libh5test library provides common support code for the tests.
  noinst_LTLIBRARIES=libh5test.la
endif

libh5test_la_SOURCES=h5test.c testframe.c cache_common.c swmr_common.c external_common.c

# Use libhd5test.la to compile all of the tests
LDADD=libh5test.la $(LIBHDF5)

# List the source files for tests that have more than one
ttsafe_SOURCES=ttsafe.c ttsafe_dcreate.c ttsafe_error.c ttsafe_cancel.c       \
               ttsafe_acreate.c ttsafe_attr_vlen.c
cache_image_SOURCES=cache_image.c genall5.c
mirror_vfd_SOURCES=mirror_vfd.c genall5.c

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
CHECK_CLEANFILES+=accum.h5 cmpd_dset.h5 mdset.h5 compact_dataset.h5 dataset.h5 dset_offset.h5 \
    max_compact_dataset.h5 simple.h5 set_local.h5 random_chunks.h5 \
    huge_chunks.h5 chunk_cache.h5 big_chunk.h5 chunk_fast.h5 chunk_expand.h5 \
    chunk_fixed.h5 copy_dcpl_newfile.h5 partial_chunks.h5 layout_extend.h5 \
    zero_chunk.h5 chunk_single.h5 swmr_non_latest.h5 \
    earray_hdr_fd.h5 farray_hdr_fd.h5 bt2_hdr_fd.h5 \
    storage_size.h5 dls_01_strings.h5 power2up.h5 version_bounds.h5 \
    alloc_0sized.h5 h5s_block.h5 h5s_plist.h5 \
    extend.h5 istore.h5 extlinks*.h5 frspace.h5 links*.h5 \
    sys_file1 tfile[1-8].h5 th5s[1-4].h5 lheap.h5 fheap.h5 ohdr.h5 \
    stab.h5 extern_[1-5].h5 extern_[1-4][rw].raw gheap[0-4].h5 \
    ohdr_min_a.h5 ohdr_min_b.h5 min_dset_ohdr_testfile.h5 \
    dt_arith[1-2] links.h5 links[0-6]*.h5 extlinks[0-15].h5 \
    tmp tmp_links tmp2_links tmp_links_env tmp_vds tmp_vds_env \
    big.data big[0-9][0-9][0-9][0-9][0-9].h5 \
    stdio.h5 sec2.h5 dtypes[0-9].h5 dtypes1[0].h5 dt_arith[1-2].h5 tattr.h5 \
    tselect.h5 mtime.h5 unlink.h5 unicode.h5 coord.h5 \
    fillval_[0-9].h5 fillval.raw mount_[0-9].h5 testmeta.h5 ttime.h5 \
    trefer[1-3].h5 trefer_*.h5 tvltypes.h5 tvlstr.h5 tvlstr2.h5 twriteorder.dat \
    flush.h5 flush-swmr.h5 noflush.h5 noflush-swmr.h5 flush_extend.h5 \
    flush_extend-swmr.h5 noflush_extend.h5 noflush_extend-swmr.h5 \
    enum1.h5 titerate.h5 ttsafe.h5 tarray1.h5 tgenprop.h5            \
    tmisc[0-9]*.h5 set_extent[1-5].h5 ext[12].bin           \
    getname.h5 getname[1-3].h5 sec2_file.h5 direct_file.h5           \
    family_file000[0-3][0-9].h5 new_family_v16-000[0-3][0-9].h5      \
    multi_file-[rs].h5 core_file filter_plugin.h5 \
    new_move_[ab].h5 ntypes.h5 dangle.h5 error_test.h5 err_compat.h5 \
    dtransform.h5 test_filters.h5 get_file_name.h5 tstint[1-2].h5    \
    unlink_chunked.h5 btree2.h5 btree2_tmp.h5 objcopy_src.h5 objcopy_dst.h5 \
    objcopy_ext.dat app_ref.h5 farray.h5 farray_tmp.h5 \
    earray.h5 earray_tmp.h5 efc[0-5].h5 log_vfd_out.log log_ros3_out.log    \
    log_s3comms_out.log new_multi_file_v16-r.h5 new_multi_file_v16-s.h5     \
    split_get_file_image_test-m.h5 split_get_file_image_test-r.h5    \
    file_image_core_test.h5.copy unregister_filter_1.h5 unregister_filter_2.h5 \
    vds_virt.h5 vds_dapl.h5 vds_src_[0-1].h5 \
    swmr_data.h5 use_use_append_chunk.h5 use_append_mchunks.h5 \
    use_disable_mdc_flushes.h5 flushrefresh.h5 flushrefresh_VERIFICATION_START \
    flushrefresh_VERIFICATION_CHECKPOINT1 flushrefresh_VERIFICATION_CHECKPOINT2 \
    flushrefresh_VERIFICATION_DONE filenotclosed.h5 del_many_dense_attrs.h5 \
    atomic_data accum_swmr_big.h5 ohdr_swmr.h5 \
    test_swmr*.h5 cache_logging.h5 cache_logging.out vds_swmr.h5 vds_swmr_src_*.h5 \
    swmr[0-2].h5 swmr_writer.out swmr_writer.log.* swmr_reader.out.* swmr_reader.log.* \
    tbogus.h5.copy cache_image_test.h5 direct_chunk.h5 native_vol_test.h5 \
    splitter*.h5 splitter.log mirror_rw mirror_ro event_set_[0-9].h5 \
    cmpd_dtransform.h5 single_latest.h5 source_file.h5 stdio_file.h5 \
    tfile_is_accessible.h5 tfile_is_accessible_non_hdf5.h5 tverbounds_dtype.h5 \
    virtual_file1.h5 tfile_double_open.h5 tfile_incr_filesize.h5 flushrefresh_test

# Sources for testhdf5 executable
testhdf5_SOURCES=testhdf5.c tarray.c tattr.c tchecksum.c tconfig.c tfile.c \
    tgenprop.c th5o.c th5s.c th5_system.c tcoords.c tid.c titerate.c tmeta.c tmisc.c \
    trefer.c trefer_deprec.c trefstr.c tselect.c tskiplist.c tsohm.c ttime.c tunicode.c \
    tvlstr.c tvltypes.c

# Sources for Use Cases
use_append_chunk_SOURCES=use_append_chunk.c use_common.c
use_append_chunk_mirror_SOURCES=use_append_chunk_mirror.c use_common.c
use_append_mchunks_SOURCES=use_append_mchunks.c use_common.c
use_disable_mdc_flushes_SOURCES=use_disable_mdc_flushes.c

# Temporary files.
DISTCLEANFILES=test_abort_fail.sh test_check_version.sh test_error.sh test_external_env.sh \
    test_flush_refresh.sh test_libinfo.sh test_links_env.sh test_plugin.sh \
    test_swmr.sh test_use_cases.sh test_vds_env.sh test_vds_swmr.sh
if MIRROR_VFD_CONDITIONAL
  DISTCLEANFILES+= test_mirror.sh
endif

include $(top_srcdir)/config/conclude.am
