$!#
$!# Copyright by The HDF Group.
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from help@hdfgroup.org.
$!#
$!
$ !
$ ! This command file tests h5diff utility. The command file has to
$ ! run in the [hdf5-top.tools.h5diff.testfiles] directory.
$ !
$ !
$ type sys$input

===================================
       Testing h5diff utiltity
===================================

$ ! Define h5diff symbol
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-11, current_dir)
$ h5diff_dir = temp + "]"
$ h5diff :== $sys$disk:'h5diff_dir'h5diff.exe
$ !

$ !
$ ! h5diff tests
$ !
$
$!# 1.0
$ CALL TOOLTEST h5diff_10.txt "-h"
$!
$!# 1.1 normal mode
$ CALL TOOLTEST h5diff_11.txt  "h5diff_basic1.h5 h5diff_basic2.h5" 
$!
$!# 1.2 normal mode with objects
$ CALL TOOLTEST h5diff_12.txt  "h5diff_basic1.h5 h5diff_basic2.h5  g1/dset1 g1/dset2"
$!
$!# 1.3 report mode
$ CALL TOOLTEST h5diff_13.txt "-r h5diff_basic1.h5 h5diff_basic2.h5"
$!
$!# 1.4 report  mode with objects
$ CALL TOOLTEST h5diff_14.txt  "-r h5diff_basic1.h5 h5diff_basic2.h5 g1/dset1 g1/dset2"
$!
$!# 1.5 with -d
$ CALL TOOLTEST h5diff_15.txt " --report --delta=5 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 1.6.1 with -p (int)
$ CALL TOOLTEST h5diff_16_1.txt "-v -p """0.02""" h5diff_basic1.h5 h5diff_basic1.h5 g1/dset5 g1/dset6"
$!
$!# 1.6.2 with -p (unsigned long long)
$ CALL TOOLTEST h5diff_16_2.txt "--verbose --relative=0.02 h5diff_basic1.h5 h5diff_basic1.h5 g1/dset7 g1/dset8"
$!
$!# 1.6.3 with -p (double)
$ CALL TOOLTEST h5diff_16_3.txt "-v -p """0.02""" h5diff_basic1.h5 h5diff_basic1.h5 g1/dset9 g1/dset10"
$!
$!# 1.7 verbose mode
$ CALL TOOLTEST h5diff_17.txt "-v h5diff_basic1.h5 h5diff_basic2.h5"  
$!
$!# 1.7 test 32-bit INFINITY
$ CALL TOOLTEST h5diff_171.txt "-v h5diff_basic1.h5 h5diff_basic1.h5 /g1/fp19 /g1/fp19_"""COPY""""
$!    
$!# 1.7 test 64-bit INFINITY
$ CALL TOOLTEST h5diff_172.txt "-v h5diff_basic1.h5 h5diff_basic1.h5 /g1/fp20 /g1/fp20_"""COPY""""
$!    
$!# 1.8 quiet mode 
$ CALL TOOLTEST h5diff_18.txt "-q h5diff_basic1.h5 h5diff_basic2.h5"
$!
$!# 1.8 -v and -q
$ CALL TOOLTEST h5diff_18_1.txt "-v -q h5diff_basic1.h5 h5diff_basic2.h5"
$!
$!
$!# ##############################################################################
$!# # not comparable types
$!# ##############################################################################
$!
$!# 2.0
$ CALL TOOLTEST h5diff_20.txt "-v h5diff_types.h5 h5diff_types.h5 dset g1"
$
$!# 2.1
$ CALL TOOLTEST h5diff_21.txt "-v h5diff_types.h5 h5diff_types.h5 dset l1"
$!
$!# 2.2
$ CALL TOOLTEST h5diff_22.txt "-v h5diff_types.h5 h5diff_types.h5 dset t1"
$!
$!# ##############################################################################
$!# # compare groups, types, links (no differences and differences)
$!# ##############################################################################
$!
$!# 2.3
$ CALL TOOLTEST h5diff_23.txt "-v h5diff_types.h5 h5diff_types.h5 g1 g1"
$!
$!# 2.4
$ CALL TOOLTEST h5diff_24.txt "-v h5diff_types.h5 h5diff_types.h5 t1 t1"
$!
$!# 2.5
$ CALL TOOLTEST h5diff_25.txt "-v h5diff_types.h5 h5diff_types.h5 l1 l1" 
$!
$!# 2.6
$ CALL TOOLTEST h5diff_26.txt "-v h5diff_types.h5 h5diff_types.h5 g1 g2"
$!
$!# 2.7
$ CALL TOOLTEST h5diff_27.txt "-v h5diff_types.h5 h5diff_types.h5 t1 t2"
$!
$!# 2.8
$ CALL TOOLTEST h5diff_28.txt "-v h5diff_types.h5 h5diff_types.h5 l1 l2"
$!
$!# ##############################################################################
$!# # Enum value tests (may become more comprehensive in the future)
$!# ##############################################################################
$!# 3.0
$!# test enum types which may have invalid values
$CALL TOOLTEST h5diff_30.txt "-v h5diff_enum_invalid_values.h5 h5diff_enum_invalid_values.h5 dset1 dset2"
$!
$!# ##############################################################################
$!# # Dataset types
$!# ##############################################################################
$
$!# 5.0
$ CALL TOOLTEST h5diff_50.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset0a dset0b"
$!
$!# 5.1
$ CALL TOOLTEST h5diff_51.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset1a dset1b"
$!
$!# 5.2
$ CALL TOOLTEST h5diff_52.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset2a dset2b"
$!
$!# 5.3
$ CALL TOOLTEST h5diff_53.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset3a dset4b"
$!
$!# 5.4
$ CALL TOOLTEST h5diff_54.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset4a dset4b"
$!
$!# 5.5
$ CALL TOOLTEST h5diff_55.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset5a dset5b"
$!
$!# 5.6
$ CALL TOOLTEST h5diff_56.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset6a dset6b"
$!
$!# 5.7
$ CALL TOOLTEST h5diff_57.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset7a dset7b"
$!
$!# 5.8 (region reference)
$ CALL TOOLTEST h5diff_58.txt "-v h5diff_dset1.h5 h5diff_dset2.h5 refreg"
$!
$!# 5.9 (test for both dset and attr with same type but with different size"
$ CALL TOOLTEST h5diff_59.txt "-v h5diff_dtypes.h5 h5diff_dtypes.h5 dset11a dset11b"
$!
$!# ##############################################################################
$!# # Error messages
$!# ##############################################################################
$!
$!
$!# 6.0: Check if the command line number of arguments is less than 3
$ CALL TOOLTEST h5diff_600.txt "h5diff_basic1.h5" 
$!
$!# 6.1: Check if non-exist object name is specified 
$ CALL TOOLTEST h5diff_601.txt "h5diff_basic1.h5 h5diff_basic1.h5 nono_obj"
$!
$!# ##############################################################################
$!# # -d 
$!# ##############################################################################
$!
$!
$!# 6.3: negative value
$ CALL TOOLTEST h5diff_603.txt "-d -4 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.4: zero
$ CALL TOOLTEST h5diff_604.txt "-d 0 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.5: non number
$ CALL TOOLTEST h5diff_605.txt "-d u h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.6: hexadecimal
$ CALL TOOLTEST h5diff_606.txt "-d 0x1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.7: string
$ CALL TOOLTEST h5diff_607.txt "-d """1""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.8: repeated option
$ CALL TOOLTEST h5diff_608.txt "-d 1 -d 2 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.9: number larger than biggest difference
$ CALL TOOLTEST h5diff_609.txt "-d 200 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.10: number smaller than smallest difference
$ CALL TOOLTEST h5diff_610.txt "-d 1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!
$!# ##############################################################################
$!# # -p
$!# ##############################################################################
$!
$!
$!
$!# 6.12: negative value
$ CALL TOOLTEST h5diff_612.txt "-p -4 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.13: zero
$ CALL TOOLTEST h5diff_613.txt "-p 0 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.14: non number
$ CALL TOOLTEST h5diff_614.txt "-p u h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.15: hexadecimal
$ CALL TOOLTEST h5diff_615.txt "-p 0x1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.16: string
$! CALL TOOLTEST h5diff_616.txt "-p """0.21""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.17: repeated option
$ CALL TOOLTEST h5diff_617.txt "-p """0.21""" -p """0.22""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.18: number larger than biggest difference
$ CALL TOOLTEST h5diff_618.txt "-p 2 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.19: number smaller than smallest difference
$ CALL TOOLTEST h5diff_619.txt "-p """0.005""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!
$!
$!# ##############################################################################
$!# # -n
$!# ##############################################################################
$!
$!
$!
$!# 6.21: negative value
$ CALL TOOLTEST h5diff_621.txt "-n -4 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.22: zero
$ CALL TOOLTEST h5diff_622.txt "-n 0 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.23: non number
$ CALL TOOLTEST h5diff_623.txt "-n u h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.24: hexadecimal
$ CALL TOOLTEST h5diff_624.txt "-n 0x1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.25: string
$ CALL TOOLTEST h5diff_625.txt "-n """2""" h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.26: repeated option
$ CALL TOOLTEST h5diff_626.txt "-n 2 -n 3 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.27: number larger than biggest difference
$ CALL TOOLTEST h5diff_627.txt "--count=200 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# 6.28: number smaller than smallest difference
$ CALL TOOLTEST h5diff_628.txt "-n 1 h5diff_basic1.h5 h5diff_basic2.h5 g1/dset3 g1/dset4"
$!
$!# ##############################################################################
$!# 6.29  non valid files
$!# ##############################################################################
$! Disable this test as C script does
$! CALL TOOLTEST h5diff_629.txt "file1.h6 file2.h6"
$!
$!# ##############################################################################
$!# # NaN
$!# ##############################################################################
$!# 6.30: test (NaN == NaN) must be true based on our documentation -- XCAO
$ CALL TOOLTEST h5diff_630.txt "-v -d """0.0001""" h5diff_basic1.h5 h5diff_basic1.h5 g1/fp18 g1/fp18_"""COPY""""
$ CALL TOOLTEST h5diff_631.txt "-v --use-system-epsilon h5diff_basic1.h5 h5diff_basic1.h5 g1/fp18 g1/fp18_"""COPY""""
$!
$!# ##############################################################################
$!# 7.  attributes
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_70.txt "-v h5diff_attr1.h5 h5diff_attr2.h5"
$!
$!# ##################################################
$!#  attrs with verbose option level
$!# ##################################################
$!
$ CALL TOOLTEST h5diff_700.txt "-v1 h5diff_attr1.h5 h5diff_attr2.h5"
$ CALL TOOLTEST h5diff_701.txt "-v2 h5diff_attr1.h5 h5diff_attr2.h5"
$ CALL TOOLTEST h5diff_702.txt "--verbose=1 h5diff_attr1.h5 h5diff_attr2.h5"
$ CALL TOOLTEST h5diff_703.txt "--verbose=2 h5diff_attr1.h5 h5diff_attr2.h5"
$!
$!# same attr number , all same attr name
$ CALL TOOLTEST h5diff_704.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5 /g"
$!
$!# same attr number , some same attr name
$ CALL TOOLTEST h5diff_705.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5 /dset"
$!
$!# same attr number , all different attr name
$ CALL TOOLTEST h5diff_706.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5 /ntype"
$!
$!# different attr number , same attr name (intersected)
$ CALL TOOLTEST h5diff_707.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5 /g2"
$!
$!# different attr number , all different attr name 
$ CALL TOOLTEST h5diff_708.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5 /g3"
$!
$!# when no attributes exist in both objects
$ CALL TOOLTEST h5diff_709.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5 /g4"
$!
$!# file vs file
$ CALL TOOLTEST h5diff_710.txt "-v2 h5diff_attr_v_level1.h5 h5diff_attr_v_level2.h5"
$!
$!# ##############################################################################
$!# 8.  all dataset datatypes
$!# ##############################################################################
$!
$ CALL TOOLTEST h5diff_80.txt "-v h5diff_dset1.h5 h5diff_dset2.h5"
$!
$!# 9. compare a file with itself
$!
$ CALL TOOLTEST h5diff_90.txt "-v h5diff_basic2.h5 h5diff_basic2.h5"
$!
$! 10. read by hyperslab, print indexes
$ CALL TOOLTEST h5diff_100.txt "-v h5diff_hyper1.h5 h5diff_hyper2.h5"
$!
$! 11. floating point comparison
$ CALL TOOLTEST h5diff_101.txt "-v h5diff_basic1.h5 h5diff_basic1.h5 g1/d1  g1/d2"
$ CALL TOOLTEST h5diff_102.txt "-v h5diff_basic1.h5 h5diff_basic1.h5 g1/fp1  g1/fp2"
$!
$!# with --use-system-epsilon for double value 
$ CALL TOOLTEST h5diff_103.txt "-v --use-system-epsilon h5diff_basic1.h5 h5diff_basic1.h5 g1/d1  g1/d2"

$!# with --use-system-epsilon for float value
$ CALL TOOLTEST h5diff_104.txt "-v --use-system-epsilon h5diff_basic1.h5 h5diff_basic1.h5 g1/fp1 g1/fp2"


$!# not comparable -c flag
$ CALL TOOLTEST h5diff_200.txt "h5diff_basic2.h5 h5diff_basic2.h5 g2/dset1  g2/dset2"
$ CALL TOOLTEST h5diff_201.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset1  g2/dset2"
$ CALL TOOLTEST h5diff_202.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset2  g2/dset3"
$ CALL TOOLTEST h5diff_203.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset3  g2/dset4"
$ CALL TOOLTEST h5diff_204.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset4  g2/dset5"
$ CALL TOOLTEST h5diff_205.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset5  g2/dset6"

$!# not comparable in compound
$ CALL TOOLTEST h5diff_206.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset7  g2/dset8"
$ CALL TOOLTEST h5diff_207.txt "-c h5diff_basic2.h5 h5diff_basic2.h5 g2/dset8  g2/dset9"

$!# not comparable in dataspace of zero dimension size
$ CALL TOOLTEST h5diff_208.txt "-c h5diff_dset_zero_dim_size1.h5 h5diff_dset_zero_dim_size2.h5"
$!
$!# non-comparable dataset with comparable attribute, and other comparable datasets.
$!# All the comparables should display differences.
$ CALL TOOLTEST h5diff_220.txt "-c non_comparables1.h5 non_comparables2.h5 /g1"
$!
$!# comparable dataset with non-comparable attribute and other comparable attributes.
$!# All the comparables should display differences.
$ CALL TOOLTEST h5diff_221.txt "-c non_comparables1.h5 non_comparables2.h5 /g2"
$!
$!# entire file
$!# All the comparables should display differences.
$ CALL TOOLTEST h5diff_222.txt "-c non_comparables1.h5 non_comparables2.h5"
$!
#!# non-comparable test for common objects (same name) with different object types
$ CALL TOOLTEST h5diff_223.txt "-c non_comparables1.h5 non_comparables2.h5 /diffobjtypes"
$!
#!# swap files
$ CALL TOOLTEST h5diff_224.txt "-c non_comparables2.h5 non_comparables1.h5 /diffobjtypes"
$!
$!# ##############################################################################
$!# # Links compare without --follow-symlinks nor --no-dangling-links
$!# ##############################################################################
$!# test for bug1749
$ CALL TOOLTEST h5diff_300.txt "-v h5diff_links.h5 h5diff_links.h5 /link_g1 /link_g2"
$!
$!# ##############################################################################
$!# # Links compare with --follow-symlinks Only
$!# ##############################################################################
$!# soft links file to file
$ CALL TOOLTEST h5diff_400.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5"
$!
$!# softlink vs dset"
$ CALL TOOLTEST h5diff_401.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset1_1 /target_dset2"
$!# dset vs softlink"
$ CALL TOOLTEST h5diff_402.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /target_dset2 /softlink_dset1_1"
$!# softlink vs softlink"
$ CALL TOOLTEST h5diff_403.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset1_1 /softlink_dset2"
$!# extlink vs extlink (FILE)"
$ CALL TOOLTEST h5diff_404.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5"
$!
$!# extlink vs dset"
$ CALL TOOLTEST h5diff_405.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_trg.h5 /ext_link_dset1 /target_group2/x_dset"
$!# dset vs extlink"
$ CALL TOOLTEST h5diff_406.txt "--follow-symlinks -v h5diff_extlink_trg.h5 h5diff_extlink_src.h5 /target_group2/x_dset /ext_link_dset1"
$!# extlink vs extlink"
$ CALL TOOLTEST h5diff_407.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_dset2"
$!# softlink vs extlink"
$ CALL TOOLTEST h5diff_408.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_extlink_src.h5 /softlink_dset1_1 /ext_link_dset2"
$!# extlink vs softlink "
$ CALL TOOLTEST h5diff_409.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_softlinks.h5 /ext_link_dset2 /softlink_dset1_1"
$!# linked_softlink vs linked_softlink (FILE)"
$ CALL TOOLTEST h5diff_410.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5"
$!
$!# dset2 vs linked_softlink_dset1"
$ CALL TOOLTEST h5diff_411.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /target_dset2 /softlink1_to_slink2"
$!# linked_softlink_dset1 vs dset2"
$ CALL TOOLTEST h5diff_412.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink1_to_slink2 /target_dset2"
$!# linked_softlink_to_dset1 vs linked_softlink_to_dset2"
$ CALL TOOLTEST h5diff_413.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink1_to_slink2 /softlink2_to_slink2"
$!# group vs linked_softlink_group1"
$ CALL TOOLTEST h5diff_414.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /target_group /softlink3_to_slink2"
$!
$!# linked_softlink_group1 vs group"
$ CALL TOOLTEST h5diff_415.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink3_to_slink2 /target_group"
$!
$!# linked_softlink_to_group1 vs linked_softlink_to_group2"
$ CALL TOOLTEST h5diff_416.txt "--follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink3_to_slink2 /softlink4_to_slink2"
$!
$!# non-exist-softlink vs softlink"
$ CALL TOOLTEST h5diff_417.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_noexist /softlink_dset2"
$!
$!# softlink vs non-exist-softlink"
$ CALL TOOLTEST h5diff_418.txt "--follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist"
$!
$!# non-exist-extlink_file vs extlink"
$ CALL TOOLTEST h5diff_419.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_noexist2 /ext_link_dset2"
$!
$!# exlink vs non-exist-extlink_file"
$ CALL TOOLTEST h5diff_420.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist2"
$!
$!# extlink vs non-exist-extlink_obj"
$ CALL TOOLTEST h5diff_421.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist1"
$!
$!# non-exist-extlink_obj vs extlink"
$ CALL TOOLTEST h5diff_422.txt "--follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_dset2"
$!
$!# extlink_to_softlink_to_dset1 vs dset2"
$ CALL TOOLTEST h5diff_423.txt "--follow-symlinks -v h5diff_ext2softlink_src.h5 h5diff_ext2softlink_trg.h5 /ext_link_to_slink1 /dset2"
$!
$!# dset2 vs extlink_to_softlink_to_dset1"
$ CALL TOOLTEST h5diff_424.txt "--follow-symlinks -v h5diff_ext2softlink_trg.h5 h5diff_ext2softlink_src.h5 /dset2 /ext_link_to_slink1"
$!
$!# extlink_to_softlink_to_dset1 vs extlink_to_softlink_to_dset2"
$ CALL TOOLTEST h5diff_425.txt "--follow-symlinks -v h5diff_ext2softlink_src.h5 h5diff_ext2softlink_src.h5 /ext_link_to_slink1 /ext_link_to_slink2"
$!
$!# ##############################################################################
$!# # Dangling links compare (--follow-symlinks and --no-dangling-links)
$!# ##############################################################################
$!# dangling links --follow-symlinks (FILE to FILE)
$ CALL TOOLTEST h5diff_450.txt  "--follow-symlinks -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5"

$!# dangling links --follow-symlinks and --no-dangling-links (FILE to FILE)
$ CALL TOOLTEST h5diff_451.txt  "--follow-symlinks -v --no-dangling-links  h5diff_danglelinks1.h5 h5diff_danglelinks2.h5"

$!# try --no-dangling-links without --follow-symlinks options
$ CALL TOOLTEST h5diff_452.txt  "--no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5"

$!# dangling link found for soft links (FILE to FILE)
$ CALL TOOLTEST h5diff_453.txt  "--follow-symlinks -v --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5"

$!# dangling link found for soft links (obj to obj)
$ CALL TOOLTEST h5diff_454.txt  "--follow-symlinks -v --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist" 
$!# dangling link found for soft links (obj to obj) Both dangle links
$ CALL TOOLTEST h5diff_455.txt  "--follow-symlinks -v --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_noexist /softlink_noexist" 
$!# dangling link found for ext links (FILE to FILE)
$ CALL TOOLTEST h5diff_456.txt  "--follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5" 
$!# dangling link found for ext links (obj to obj). target file exist
$ CALL TOOLTEST h5diff_457.txt  "--follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist1" 
$!# dangling link found for ext links (obj to obj). target file NOT exist
$ CALL TOOLTEST h5diff_458.txt  "--follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist2"  
$!# dangling link found for ext links (obj to obj). Both dangle links
$ CALL TOOLTEST h5diff_459.txt  "--follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_noexist2"
$!
#!# dangling link --follow-symlinks (obj vs obj)
$ CALL TOOLTEST h5diff_465.txt  "--follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1"
#!# soft dangling vs. soft dangling
$ CALL TOOLTEST h5diff_466.txt  "--follow-symlinks -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1"
#!# soft link  vs. soft dangling
$ CALL TOOLTEST h5diff_467.txt  "--follow-symlinks -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link2"
#!# ext dangling vs. ext dangling
$ CALL TOOLTEST h5diff_468.txt  "--follow-symlinks -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link4"
#!# ext link vs. ext dangling
$ CALL TOOLTEST h5diff_469.txt  "--follow-symlinks -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link2"
$!
#!# dangling links without follow symlink
#!# test - soft dangle links (same and different paths),
#!#      - external dangle links (same and different paths)
$ CALL TOOLTEST h5diff_471.txt  "-v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5"
$ CALL TOOLTEST h5diff_472.txt  "-v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1"
$ CALL TOOLTEST h5diff_473.txt  "-v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link4"
$ CALL TOOLTEST h5diff_474.txt  "-v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link4"
$ CALL TOOLTEST h5diff_475.txt  "-v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link1"

$!# ##############################################################################
$!# # test for group diff recursivly
$!# ##############################################################################
$!# root 
$ CALL TOOLTEST h5diff_500.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 / /"
$ CALL TOOLTEST h5diff_501.txt "-v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 / /"

$!# root vs group (It doesn't work on VMS.  Debug it in the future)
$! CALL TOOLTEST h5diff_502.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 / /grp1/grp2/grp3"

$!# group vs group (same name and structure)
$ CALL TOOLTEST h5diff_503.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /grp1"

$!# group vs group (different name and structure)
$ CALL TOOLTEST h5diff_504.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1/grp2 /grp1/grp2/grp3"

$!# groups vs soft-link
$ CALL TOOLTEST h5diff_505.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /slink_grp1"
$ CALL TOOLTEST h5diff_506.txt "-v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1/grp2 /slink_grp2"

$!# groups vs ext-link
$ CALL TOOLTEST h5diff_507.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /elink_grp1" 
$ CALL TOOLTEST h5diff_508.txt "-v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /elink_grp1"

$!# soft-link vs ext-link 
$ CALL TOOLTEST h5diff_509.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp1 /elink_grp1"
$ CALL TOOLTEST h5diff_510.txt "-v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp1 /elink_grp1"

$!# circled ext links
$ CALL TOOLTEST h5diff_511.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp10 /grp11"
$ CALL TOOLTEST h5diff_512.txt "-v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp10 /grp11"

$!# circled soft2ext-link vs soft2ext-link
$ CALL TOOLTEST h5diff_513.txt "-v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp10 /slink_grp11"
$ CALL TOOLTEST h5diff_514.txt "-v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp10 /slink_grp11"
$!
$!###############################################################################
$!# Test for group recursive diff via multi-linked external links 
$!# With follow-symlinks, file h5diff_grp_recurse_ext1.h5 and h5diff_grp_recurse_ext2-1.h5 should
$!# be same with the external links.
$!###############################################################################
$!# file vs file
$ CALL TOOLTEST h5diff_515.txt "-v h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5"
$ CALL TOOLTEST h5diff_516.txt "-v --follow-symlinks h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5"
$!# group vs group
$ CALL TOOLTEST h5diff_517.txt "-v h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5 /g1"
$ CALL TOOLTEST h5diff_518.txt "-v --follow-symlinks h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5 /g1"

$!# ##############################################################################
$!# # Exclude objects (--exclude-path)
$!# ##############################################################################
$!#
$!# Same structure, same names and different value.
$!#
$!# Exclude the object with different value. Expect return - same
$ CALL TOOLTEST h5diff_480.txt "-v --exclude-path /group1/dset3 h5diff_exclude1-1.h5 h5diff_exclude1-2.h5"
$!# Verify different by not excluding. Expect return - diff
$ CALL TOOLTEST h5diff_481.txt "-v h5diff_exclude1-1.h5 h5diff_exclude1-2.h5"

$!#
$!# Different structure, different names. 
$!#
$!# Exclude all the different objects. Expect return - same
$ CALL TOOLTEST h5diff_482.txt "-v --exclude-path "/group1" --exclude-path "/dset1" h5diff_exclude2-1.h5 h5diff_exclude2-2.h5"
$!# Exclude only some different objects. Expect return - diff
$ CALL TOOLTEST h5diff_483.txt "-v --exclude-path "/group1" h5diff_exclude2-1.h5 h5diff_exclude2-2.h5"

$!# Exclude from group compare
$ CALL TOOLTEST h5diff_484.txt "-v --exclude-path "/dset3" h5diff_exclude1-1.h5 h5diff_exclude1-2.h5 /group1"

$!# Only one file contains unique objs. Common objs are same.
$ CALL TOOLTEST h5diff_485.txt "-v --exclude-path "/group1" h5diff_exclude3-1.h5 h5diff_exclude3-2.h5"
$ CALL TOOLTEST h5diff_486.txt "-v --exclude-path "/group1" h5diff_exclude3-2.h5 h5diff_exclude3-1.h5"
$ CALL TOOLTEST h5diff_487.txt "-v --exclude-path "/group1/dset" h5diff_exclude3-1.h5 h5diff_exclude3-2.h5"

$!# ##############################################################################
$!# # diff various multiple vlen and fixed strings in a compound type dataset
$!# ############################################################################## 
$ CALL TOOLTEST h5diff_530.txt "-v  h5diff_comp_vl_strs.h5 h5diff_comp_vl_strs.h5 /group /group_copy"

$!# ##############################################################################
$!# # Test container types (array,vlen) with multiple nested compound types
$!# # Complex compound types in dataset and attribute
$!# ##############################################################################
$ CALL TOOLTEST h5diff_540.txt "-v compounds_array_vlen1.h5 compounds_array_vlen2.h5"
$!
$!# ##############################################################################
$!# # Test mutually exclusive options 
$!# ##############################################################################
$!# Test with -d , -p and --use-system-epsilon. 
$ CALL TOOLTEST h5diff_640.txt "-v -d 5 -p """0.05""" --use-system-epsilon h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$ CALL TOOLTEST h5diff_641.txt "-v -d 5 -p """0.05""" h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$ CALL TOOLTEST h5diff_642.txt "-v -p """0.05""" -d 5 h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$ CALL TOOLTEST h5diff_643.txt "-v -d 5 --use-system-epsilon h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$ CALL TOOLTEST h5diff_644.txt "-v --use-system-epsilon -d 5 h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$ CALL TOOLTEST h5diff_645.txt "-v -p """0.05""" --use-system-epsilon h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$ CALL TOOLTEST h5diff_646.txt "-v --use-system-epsilon -p """0.05""" h5diff_basic1.h5 h5diff_basic2.h5 /g1/dset3 /g1/dset4"
$!
$!# ##############################################################################
$!# # END
$!# ##############################################################################
$!
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "h5diffout"
$ actual_err = base + "h5differr"
$
$ begin = "Testing h5diff "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ ON ERROR THEN CONTINUE
$ h5diff 'P2
$ deassign sys$output
$ deassign sys$error
$ if F$SEARCH(actual_err) .NES. ""
$ then
$ set message/notext/nofacility/noidentification/noseverity
$    append 'actual_err' 'actual'
$ set message/text/facility/identification/severity
$ endif
$ !
$ ! Compare the results
$ !
$ diff/output=h5diff_temp/ignore=(spacing,trailing_spaces,blank_lines)/comment_delim=(%) 'actual' 'P1'
$ open/read temp_out h5diff_temp.dif
$ read temp_out record1
$ close temp_out
$ !
$ ! Extract error code and format output line
$ !
$ len = F$LENGTH(record1)
$ err_code = F$EXTRACT(len-1,1,record1)
$ if err_code .eqs. "0" 
$  then
$    result = "PASSED"
$    line = F$FAO("!15AS !50AS !70AS", begin, P2, result) 
$  else
$    result = "*FAILED*"
$    line = F$FAO("!15AS !49AS !69AS", begin, P2, result) 
$ endif
$ !
$ ! Print test result
$ ! 
$  write sys$output line
$ ! 
$ ! Append the result to the log file 
$ !
$ append/new_version h5diff_temp.dif h5diff.log
$ append/new_version 'actual'        h5diff_output.txt
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_err)  .NES. ""
$ then
$  del *.h5differr;*
$ endif
$  del *.h5diffout;*
$  del h5diff_temp.dif;*
$ !
$ENDSUBROUTINE

