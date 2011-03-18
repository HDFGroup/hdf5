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
    H5DIFF-h5diff_10      #uses runTest.cmake
    H5DIFF-h5diff_11      #uses runTest.cmake
    H5DIFF-h5diff_12      #uses runTest.cmake
    H5DIFF-h5diff_13      #uses runTest.cmake
    H5DIFF-h5diff_14      #uses runTest.cmake
    H5DIFF-h5diff_15      #uses runTest.cmake
    H5DIFF-h5diff_16_1    #uses runTest.cmake
    H5DIFF-h5diff_16_2    #uses runTest.cmake
    H5DIFF-h5diff_16_3    #uses runTest.cmake
    H5DIFF-h5diff_17      #uses runTest.cmake
    H5DIFF-h5diff_171     #uses runTest.cmake
    H5DIFF-h5diff_172     #uses runTest.cmake
    H5DIFF-h5diff_18      #uses runTest.cmake
    H5DIFF-h5diff_18_1    #uses runTest.cmake
    H5DIFF-h5diff_20      #uses runTest.cmake
    H5DIFF-h5diff_21      #uses runTest.cmake
    H5DIFF-h5diff_22      #uses runTest.cmake
    H5DIFF-h5diff_23      #uses runTest.cmake
    H5DIFF-h5diff_24      #uses runTest.cmake
    H5DIFF-h5diff_25      #uses runTest.cmake
    H5DIFF-h5diff_26      #uses runTest.cmake
    H5DIFF-h5diff_27      #uses runTest.cmake
    H5DIFF-h5diff_28      #uses runTest.cmake
    H5DIFF-h5diff_50      #uses runTest.cmake
    H5DIFF-h5diff_51      #uses runTest.cmake
    H5DIFF-h5diff_52      #uses runTest.cmake
    H5DIFF-h5diff_53      #uses runTest.cmake
    H5DIFF-h5diff_54      #uses runTest.cmake
    H5DIFF-h5diff_55      #uses runTest.cmake
    H5DIFF-h5diff_56      #uses runTest.cmake
    H5DIFF-h5diff_57      #uses runTest.cmake
    H5DIFF-h5diff_58      #uses runTest.cmake
    H5DIFF-h5diff_600     #uses runTest.cmake
    H5DIFF-h5diff_601     #uses runTest.cmake
    H5DIFF-h5diff_603     #uses runTest.cmake
    H5DIFF-h5diff_604     #uses runTest.cmake
    H5DIFF-h5diff_605     #uses runTest.cmake
    H5DIFF-h5diff_606     #uses runTest.cmake
    H5DIFF-h5diff_607     #uses runTest.cmake
    H5DIFF-h5diff_608     #uses runTest.cmake
    H5DIFF-h5diff_609     #uses runTest.cmake
    H5DIFF-h5diff_610     #uses runTest.cmake
    H5DIFF-h5diff_612     #uses runTest.cmake
    H5DIFF-h5diff_613     #uses runTest.cmake
    H5DIFF-h5diff_614     #uses runTest.cmake
    H5DIFF-h5diff_615     #uses runTest.cmake
    H5DIFF-h5diff_616     #uses runTest.cmake
    H5DIFF-h5diff_617     #uses runTest.cmake
    H5DIFF-h5diff_618     #uses runTest.cmake
    H5DIFF-h5diff_619     #uses runTest.cmake
    H5DIFF-h5diff_621     #uses runTest.cmake
    H5DIFF-h5diff_622     #uses runTest.cmake
    H5DIFF-h5diff_623     #uses runTest.cmake
    H5DIFF-h5diff_624     #uses runTest.cmake
    H5DIFF-h5diff_625     #uses runTest.cmake
    H5DIFF-h5diff_626     #uses runTest.cmake
    H5DIFF-h5diff_627     #uses runTest.cmake
    H5DIFF-h5diff_628     #uses runTest.cmake
    H5DIFF-h5diff_70      #uses runTest.cmake
    H5DIFF-h5diff_80      #uses runTest.cmake
    H5DIFF-h5diff_90      #uses runTest.cmake
    H5DIFF-h5diff_101     #uses runTest.cmake
    H5DIFF-h5diff_102     #uses runTest.cmake
    H5DIFF-h5diff_200     #uses runTest.cmake
    H5DIFF-h5diff_201     #uses runTest.cmake
    H5DIFF-h5diff_202     #uses runTest.cmake
    H5DIFF-h5diff_203     #uses runTest.cmake
    H5DIFF-h5diff_204     #uses runTest.cmake
    H5DIFF-h5diff_205     #uses runTest.cmake
    H5DIFF-h5diff_206     #uses runTest.cmake
    H5DIFF-h5diff_207     #uses runTest.cmake
    H5DIFF-h5diff_300     #uses runTest.cmake
    H5DIFF-h5diff_400     #uses runTest.cmake
    H5DIFF-h5diff_401     #uses runTest.cmake
    H5DIFF-h5diff_402     #uses runTest.cmake
    H5DIFF-h5diff_403     #uses runTest.cmake
    H5DIFF-h5diff_404     #uses runTest.cmake
    H5DIFF-h5diff_405     #uses runTest.cmake
    H5DIFF-h5diff_406     #uses runTest.cmake
    H5DIFF-h5diff_407     #uses runTest.cmake
    H5DIFF-h5diff_408     #uses runTest.cmake
    H5DIFF-h5diff_409     #uses runTest.cmake
    H5DIFF-h5diff_410     #uses runTest.cmake
    H5DIFF-h5diff_411     #uses runTest.cmake
    H5DIFF-h5diff_412     #uses runTest.cmake
    H5DIFF-h5diff_413     #uses runTest.cmake
    H5DIFF-h5diff_414     #uses runTest.cmake
    H5DIFF-h5diff_415     #uses runTest.cmake
    H5DIFF-h5diff_416     #uses runTest.cmake
    H5DIFF-h5diff_417     #uses runTest.cmake
    H5DIFF-h5diff_418     #uses runTest.cmake
    H5DIFF-h5diff_419     #uses runTest.cmake
    H5DIFF-h5diff_420     #uses runTest.cmake
    H5DIFF-h5diff_421     #uses runTest.cmake
    H5DIFF-h5diff_422     #uses runTest.cmake
    H5DIFF-h5diff_423     #uses runTest.cmake
    H5DIFF-h5diff_424     #uses runTest.cmake
    H5DIFF-h5diff_425     #uses runTest.cmake
    H5DIFF-h5diff_450     #uses runTest.cmake
    H5DIFF-h5diff_451     #uses runTest.cmake
    H5DIFF-h5diff_452     #uses runTest.cmake
    H5DIFF-h5diff_453     #uses runTest.cmake
    H5DIFF-h5diff_454     #uses runTest.cmake
    H5DIFF-h5diff_455     #uses runTest.cmake
    H5DIFF-h5diff_456     #uses runTest.cmake
    H5DIFF-h5diff_457     #uses runTest.cmake
    H5DIFF-h5diff_458     #uses runTest.cmake
    H5DIFF-h5diff_459     #uses runTest.cmake
    H5DIFF-h5diff_500     #uses runTest.cmake
    H5DIFF-h5diff_501     #uses runTest.cmake
    H5DIFF-h5diff_502     #uses runTest.cmake
    H5DIFF-h5diff_503     #uses runTest.cmake
    H5DIFF-h5diff_504     #uses runTest.cmake
    H5DIFF-h5diff_505     #uses runTest.cmake
    H5DIFF-h5diff_506     #uses runTest.cmake
    H5DIFF-h5diff_507     #uses runTest.cmake
    H5DIFF-h5diff_508     #uses runTest.cmake
    H5DIFF-h5diff_509     #uses runTest.cmake
    H5DIFF-h5diff_510     #uses runTest.cmake
    H5DIFF-h5diff_511     #uses runTest.cmake
    H5DIFF-h5diff_512     #uses runTest.cmake
    H5DIFF-h5diff_513     #uses runTest.cmake
    H5DIFF-h5diff_514     #uses runTest.cmake
    H5DIFF-h5diff_515     #uses runTest.cmake
    H5DIFF-h5diff_516     #uses runTest.cmake
    H5DIFF-h5diff_517     #uses runTest.cmake
    H5DIFF-h5diff_518     #uses runTest.cmake
    H5DIFF-h5diff_480     #uses runTest.cmake
    H5DIFF-h5diff_481     #uses runTest.cmake
    H5DIFF-h5diff_482     #uses runTest.cmake
    H5DIFF-h5diff_483     #uses runTest.cmake
    H5DIFF-h5diff_484     #uses runTest.cmake
    H5DIFF-h5diff_530     #uses runTest.cmake
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
    H5STAT-h5stat_help1            #uses runTest.cmake
    H5STAT-h5stat_help2            #uses runTest.cmake
    H5STAT-h5stat_filters          #uses runTest.cmake
    H5STAT-h5stat_filters-file     #uses runTest.cmake
    H5STAT-h5stat_filters-F        #uses runTest.cmake
    H5STAT-h5stat_filters-d        #uses runTest.cmake
    H5STAT-h5stat_filters-g        #uses runTest.cmake
    H5STAT-h5stat_filters-dT       #uses runTest.cmake
    H5STAT-h5stat_filters-UD       #uses runTest.cmake
    H5STAT-h5stat_filters-UT       #uses runTest.cmake
    H5STAT-h5stat_tsohm            #uses runTest.cmake
    H5STAT-h5stat_newgrat          #uses runTest.cmake
    H5STAT-h5stat_newgrat-UG       #uses runTest.cmake
    H5STAT-h5stat_newgrat-UA       #uses runTest.cmake
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
