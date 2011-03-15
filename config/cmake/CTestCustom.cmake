SET (CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS 1500)
 
SET (CTEST_CUSTOM_WARNING_EXCEPTION
    ${CTEST_CUSTOM_WARNING_EXCEPTION}
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning C4090"
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning:[ \t]*passing argument"
    "H5detect.c[0-9 \t:]*warning:[ \t]*passing argument"
    "H5Tconv.c[0-9 \t:]*warning:[ \t]*comparison is always false due to limited range of data type"
    "testhdf5.h.[0-9]+.[ \t]*:[ \t]*warning C4005"
    "H5Ztrans.c.[0-9]+.[ \t]*:[ \t]*warning C4244"
    "SZIP.src.*:[ \t]*warning"
    "POSIX name for this item is deprecated"
    "disabling jobserver mode"
)
 
SET (CTEST_CUSTOM_MEMCHECK_IGNORE
    ${CTEST_CUSTOM_MEMCHECK_IGNORE}
    flush1           #designed to fail
    flush2           #designed to need flush1
    error_test       #uses runTest.cmake
    err_compat       #uses runTest.cmake
    links_env        #uses runTest.cmake
    h5test-clear-objects
    h5perform-clear-objects
    hl_test-clear-objects
    hl_fortran_test-clear-objects
    ######### tools/h5copy #########
    H5COPY-clearall-objects
    H5COPY-H5LS_h5copytst-basic       #uses runTest.cmake
    H5COPY-clear-refs
    H5COPY-region_ref                 #needs clear-refs
    H5COPY-H5LS_h5copy_ref-refs       #uses runTest.cmake
    H5COPY-clear-ext-links
    H5COPY-ext_link                   #needs clear-ext-links
    H5COPY-ext_link_f                 #needs clear-ext-links
    H5COPY-ext_dangle_noobj           #needs clear-ext-links
    H5COPY-ext_dangle_noobj_f         #needs clear-ext-links
    H5COPY-ext_dangle_nofile          #needs clear-ext-links
    H5COPY-ext_dangle_nofile_f        #needs clear-ext-links
    H5COPY-ext_link_group             #needs clear-ext-links
    H5COPY-ext_link_group_f           #needs clear-ext-links
    H5COPY-H5LS_h5copy_extlinks_src-links    #uses runTest.cmake
    H5COPY-clear-misc
    H5COPY-CMP-h5copy_misc1           #uses runTest.cmake
    H5COPY-samefile1_pre              #needs clear-ext-links
    H5COPY-samefile2_pre              #needs clear-ext-links
    H5COPY-samefile1                  #needs clear-ext-links
    H5COPY-samefile2                  #needs clear-ext-links
    ######### tools/h5diff #########
    H5DIFF-clearall-objects
    ######### tools/h5dump #########
    H5DUMP-clearall-objects
    H5DUMP-clear-out1
    H5DUMP-clear-out3
    H5DUMP-clear-objects
    H5DUMP_PACKED_BITS-clearall-objects
    H5DUMP-XML-clearall-objects
    ######### tools/h5import #########
    H5IMPORT-clear-objects
    ######### tools/h5ls #########
    H5LS-clearall-objects
    ######### tools/h5repack #########
    H5REPACK-clearall-objects
    H5REPACK-gzip_verbose_filters                 #uses runTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_chunk_20x10            #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-chunk_20x10              #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-conti                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset2_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-compa                    #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_compa_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_compa             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_chunk             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-dset_conti_conti             #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_compa                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_conti                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-chunk_18x13                  #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_compa           #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT-contig_small_fixed_compa     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_long_switches     #uses grepTest.cmake
    H5REPACK_VERIFY_LAYOUT_ALL-layout_short_switches    #uses grepTest.cmake
    ######### tools/h5stat #########
    H5STAT-clearall-objects
    ######### tools/misc #########
    h5repart_20K-clear-objects
    h5repart_5K-clear-objects
    h5repart_sec2-clear-objects
    H5MKGRP-h5mkgrp_help                                #uses runTest.cmake
    H5MKGRP-h5mkgrp_version                             #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single
    H5MKGRP-h5mkgrp_single                              #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single                         #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single-v
    H5MKGRP-h5mkgrp_single-v                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single-v                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single-p
    H5MKGRP-h5mkgrp_single-p                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single-p                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_single_latest-l
    H5MKGRP-h5mkgrp_single_latest-l                     #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_single_latest-l                #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several
    H5MKGRP-h5mkgrp_several                             #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several                        #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several-v
    H5MKGRP-h5mkgrp_several-v                           #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several-v                      #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several-p
    H5MKGRP-h5mkgrp_several-p                           #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several-p                      #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_several_latest-l
    H5MKGRP-h5mkgrp_several_latest-l                    #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_several_latest-l               #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested-p
    H5MKGRP-h5mkgrp_nested-p                            #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested-p                       #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_latest-lp
    H5MKGRP-h5mkgrp_nested_latest-lp                    #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_latest-lp               #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_mult-p
    H5MKGRP-h5mkgrp_nested_mult-p                       #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_mult-p                  #uses runTest.cmake
    H5MKGRP-clear-h5mkgrp_nested_mult_latest-lp
    H5MKGRP-h5mkgrp_nested_mult_latest-lp               #uses runTest.cmake
    H5MKGRP-H5LS-h5mkgrp_nested_mult_latest-lp          #uses runTest.cmake
)
