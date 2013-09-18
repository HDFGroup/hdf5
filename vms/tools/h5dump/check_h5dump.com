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
$ !
$ ! This command file tests h5dump utility. The command file has to
$ ! run in the [hdf5-top.tools.testfiles] directory.
$ !
$ type sys$input

===================================
       Testing h5dump utiltity
===================================

$
$ !
$ ! Define h5dump symbol
$ !
$! set message/notext/nofacility/noidentification/noseverity
$ current_dir = F$DIRECTRY()
$ len = F$LENGTH(current_dir)
$ temp = F$EXTRACT(0, len-10, current_dir)
$ h5dump_dir = temp + "H5DUMP]"
$ h5dump :== $sys$disk:'h5dump_dir'h5dump.exe
$ !
$ ! h5dump tests
$ !
$
$ ! Test data output redirection
$ CALL TOOLTEST tnoddl.ddl "--ddl -y packedbits.h5"
$ CALL TOOLTEST tnodata.ddl "--output packedbits.h5"
$ CALL TOOLTEST tnoattrddl.ddl "-"""O""" -y tattr.h5"
$ CALL TOOLTEST tnoattrdata.ddl "-"""A""" -o tattr.h5"
$! These 4 cases need new function to handle them.  I run them temporarily
$! with TOOLTEST to check the metadata part and left out the data part.
$ CALL TOOLTEST trawdatafile.ddl "-y -o trawdatafile.txt packedbits.h5"
$ CALL TOOLTEST tnoddlfile.ddl "-"""O""" -y -o tnoddlfile.txt packedbits.h5"
$! CALL TOOLTEST2A twithddlfile.exp twithddl.exp "--ddl=twithddl.txt -y -o twithddlfile.txt packedbits.h5"
$ CALL TOOLTEST trawssetfile.ddl "-d "/dset1[1,1;;;]" -y -o trawssetfile.txt tdset.h5"
$
$ ! Test for maximum display datasets
$ CALL TOOLTEST twidedisplay.ddl "-w0 packedbits.h5"
$
$ ! Test for signed/unsigned datasets 
$ CALL TOOLTEST packedbits.ddl "packedbits.h5"
$ ! Test for compound signed/unsigned datasets
$ CALL TOOLTEST tcmpdintsize.ddl "tcmpdintsize.h5"
$ ! Test for signed/unsigned scalar datasets
$ CALL TOOLTEST tscalarintsize.ddl "tscalarintsize.h5"
$ ! Test for signed/unsigned attributes
$ CALL TOOLTEST tattrintsize.ddl "tattrintsize.h5"
$ ! Test for compound signed/unsigned attributes
$ CALL TOOLTEST tcmpdattrintsize.ddl "tcmpdattrintsize.h5"
$ ! Test for signed/unsigned scalar attributes
$ CALL TOOLTEST tscalarattrintsize.ddl "tscalarattrintsize.h5"
$ ! Test for string scalar dataset attribute
$ CALL TOOLTEST tscalarstring.ddl "tscalarstring.h5"
$
$ ! Test for displaying groups
$ CALL TOOLTEST tgroup-1.ddl "tgroup.h5"
$ ! Test for displaying the selected groups
$ ! Commented out due to the difference of printing format.
$ ! CALL TOOLTEST tgroup-2.ddl "--group=/g2 --group / -g /y tgroup.h5"
$
$ ! Test for displaying simple space datasets
$ CALL TOOLTEST tdset-1.ddl "tdset.h5"
$ ! Test for displaying selected datasets
$ ! Commented out due to the difference of printing format.
$ ! CALL TOOLTEST tdset-2.ddl "-"""H""" -d dset1 -d /dset2 --dataset=dset3 tdset.h5"
$
$ ! Test for displaying attributes
$ CALL TOOLTEST tattr-1.ddl "tattr.h5"
$ ! Test for displaying the selected attributes of string type and scalar space
$ ! Commented out due to the difference of printing format
$ ! CALL TOOLTEST tattr-2.ddl "-a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5"
$ ! Test for header and error messages
$ ! Commented out due to the difference of printing format.
$ ! CALL TOOLTEST tattr-3.ddl "--header -a /attr2 --attribute=/attr tattr.h5"
$ ! Test for displaying at least 9 attributes on root from a BE machine
$ CALL TOOLTEST tattr-4_be.ddl "tattr4_be.h5"
$ ! Test for displaying attributes in shared datatype (also in group and dataset)
$ CALL TOOLTEST tnamed_dtype_attr.ddl "tnamed_dtype_attr.h5"
$
$ ! Test for displaying soft links and user-defined links
$ CALL TOOLTEST tslink-1.ddl "tslink.h5"
$ CALL TOOLTEST tudlink-1.ddl "tudlink.h5"
$ ! Test for displaying the selected link
$ CALL TOOLTEST tslink-2.ddl "-l slink2 tslink.h5"
$ CALL TOOLTEST tudlink-2.ddl "-l udlink2 tudlink.h5"
$ ! Test for displaying dangling soft links
$ CALL TOOLTEST tslink-D.ddl "-d /slink1 tslink.h5"
$
$ ! Tests for hard links
$ CALL TOOLTEST thlink-1.ddl "thlink.h5"
$ CALL TOOLTEST thlink-2.ddl "-d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5"
$ CALL TOOLTEST thlink-3.ddl "-d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5"
$ CALL TOOLTEST thlink-4.ddl "-g /g1 thlink.h5"
$ CALL TOOLTEST thlink-5.ddl "-d /dset1 -g /g2 -d /g1/dset2 thlink.h5"
$
$ ! Tests for compound data types
$ CALL TOOLTEST tcomp-1.ddl "tcompound.h5"
$ ! test for named data types
$ CALL TOOLTEST tcomp-2.ddl "-t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5"
$ ! Test for unamed type 
$ CALL TOOLTEST tcomp-3.ddl "-t /#6632 -g /group2 tcompound.h5"
$ ! Test complicated compound datatype
$ CALL TOOLTEST tcomp-4.ddl "tcompound_complex.h5"
$
$ ! Test for the nested compound type
$ CALL TOOLTEST tnestcomp-1.ddl "tnestedcomp.h5"
$ CALL TOOLTEST tnestedcmpddt.ddl "tnestedcmpddt.h5"
$
$ ! test for options
$ CALL TOOLTEST tall-1.ddl "tall.h5"
$ CALL TOOLTEST tall-2.ddl "--header -g /g1/g1.1 -a attr2 tall.h5"
$ CALL TOOLTEST tall-3.ddl "-d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5"
$
$ ! Test for loop detection
$ CALL TOOLTEST tloop-1.ddl "tloop.h5"
$
$ ! Test for string 
$ CALL TOOLTEST tstr-1.ddl "tstr.h5"
$ CALL TOOLTEST tstr-2.ddl "tstr2.h5"
$
$ ! Test for file created by Lib SAF team
$ CALL TOOLTEST tsaf.ddl "tsaf.h5"
$
$ ! Test for file with variable length data
$ CALL TOOLTEST tvldtypes1.ddl "tvldtypes1.h5"
$ CALL TOOLTEST tvldtypes2.ddl "tvldtypes2.h5"
$ CALL TOOLTEST tvldtypes3.ddl "tvldtypes3.h5"
$ CALL TOOLTEST tvldtypes4.ddl "tvldtypes4.h5"
$ CALL TOOLTEST tvldtypes5.ddl "tvldtypes5.h5"
$
$ ! Test for file with variable length string data
$ CALL TOOLTEST tvlstr.ddl "tvlstr.h5"
$
$ ! Test for files with array data
$ CALL TOOLTEST tarray1.ddl "tarray1.h5"
$ ! Added for bug# 2092 - tarray1_big.h
$ CALL TOOLTEST tarray1_big.ddl "-"""R""" tarray1_big.h5"
$ CALL TOOLTEST tarray2.ddl "tarray2.h5"
$ CALL TOOLTEST tarray3.ddl "tarray3.h5"
$ CALL TOOLTEST tarray4.ddl "tarray4.h5"
$ CALL TOOLTEST tarray5.ddl "tarray5.h5"
$ CALL TOOLTEST tarray6.ddl "tarray6.h5"
$ CALL TOOLTEST tarray7.ddl "tarray7.h5"
$ CALL TOOLTEST tarray8.ddl "tarray8.h5"
$
$ ! Test for files with empty data
$ CALL TOOLTEST tempty.ddl "tempty.h5"
$
$ ! Test for files with groups that have comments
$ CALL TOOLTEST tgrp_comments.ddl "tgrp_comments.h5"
$
$ ! Test the --filedriver flag
$ CALL TOOLTEST tsplit_file.ddl "--filedriver=split tsplit_file"
$ CALL TOOLTEST tfamily.ddl "--filedriver=family tfamily%05d.h5"
$ CALL TOOLTEST tmulti.ddl "--filedriver=multi tmulti"
$
$ ! Test for files with group names which reach > 1024 bytes in size
$ CALL TOOLTEST tlarge_objname.ddl "-w157 tlarge_objname.h5"
$
$ ! Test '-A' to suppress data but print attr's
$ CALL TOOLTEST tall-2A.ddl "-"""A""" tall.h5"
$
$ ! Test '-r' to print attributes in ASCII instead of decimal
$ CALL TOOLTEST tall-2B.ddl "-"""A""" -r tall.h5"
$
$ ! Test Subsetting
$ CALL TOOLTEST tall-4s.ddl "--dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5"
$ CALL TOOLTEST tall-5s.ddl "-d /g1/g1.1/dset1.1.2[0;2;10;] tall.h5"
$ CALL TOOLTEST tdset-3s.ddl "-d /dset1[1,1;;;] tdset.h5"
$! CALL TOOLTEST tdset-3s.ddl "-d """/"dset"1[;3,2;4,4;1,4]""" tdset2.h5"
$
$ ! Test printing characters in ASCII instead of decimal
$ CALL TOOLTEST tchar1.ddl "-r tchar.h5"
$
$ ! Test datatypes in ASCII and UTF8
$ CALL TOOLTEST charsets.ddl "charsets.h5"
$
$ ! rev. 2004
$
$ ! Tests for super block
$ CALL TOOLTEST tboot1.ddl "-"""H""" -"""B""" -d dset tfcontents1.h5"
$ CALL TOOLTEST tboot2.ddl "-"""B""" tfcontents2.h5"
$
$ ! Test -p with a non existing dataset
$ ! Commented out due to the difference of printing format
$ ! CALL TOOLTEST tperror.ddl "-p -d bogus tfcontents1.h5"
$
$ ! Test for file contents
$ CALL TOOLTEST tcontents.ddl "-n tfcontents1.h5"
$ CALL TOOLTEST tordercontents1.ddl "-n --sort_by=name --sort_order=ascending tfcontents1.h5'
$ CALL TOOLTEST tordercontents2.ddl "-n --sort_by=name --sort_order=descending tfcontents1.h5"
$ CALL TOOLTEST tattrcontents1.ddl "-n 1 --sort_order=ascending tall.h5"
$ CALL TOOLTEST tattrcontents2.ddl "-n 1 --sort_order=descending tall.h5"
$
$ ! Tests for storage layout
$ ! Compact
$ CALL TOOLTEST tcompact.ddl "-"""H""" -p -d compact tfilters.h5"
$ ! Contiguous
$ CALL TOOLTEST tcontiguos.ddl "-"""H""" -p -d contiguous tfilters.h5"
$ ! Chunked
$ CALL TOOLTEST tchunked.ddl "-"""H""" -p -d chunked tfilters.h5"
$ ! External 
$ CALL TOOLTEST texternal.ddl "-"""H""" -p -d external tfilters.h5"
$
$ ! Fill values
$ CALL TOOLTEST tfill.ddl "-p tfvalues.h5"
$
$ ! Several datatype, with references , print path
$ CALL TOOLTEST treference.ddl  "tattr2.h5"
$
$ ! Escape/not escape non printable characters
$ CALL TOOLTEST tstringe.ddl "-e tstr3.h5"
$ CALL TOOLTEST tstring.ddl "tstr3.h5"
$ ! Char data as ASCII with non escape
$ CALL TOOLTEST tstring2.ddl "-r -d str4 tstr3.h5"
$
$ ! Array indices print/not print
$ CALL TOOLTEST tindicesyes.ddl "taindices.h5"
$ CALL TOOLTEST tindicesno.ddl "-y taindices.h5"
$
$ ! Array indices with subsetting
$ ! 1D case
$ CALL TOOLTEST tindicessub1.ddl "-d 1d -s 1 -"""S""" 10 -c 2 -k 3 taindices.h5"
$ ! 2D case
$ CALL TOOLTEST tindicessub2.ddl "-d 2d -s 1,2 -"""S""" 3,3 -c 3,2 -k 2,2 taindices.h5"
$ ! 3D case
$ CALL TOOLTEST tindicessub3.ddl "-d 3d -s 0,1,2 -"""S""" 1,3,3 -c 2,2,2 -k 1,2,2 taindices.h5"
$ ! 4D case
$ CALL TOOLTEST tindicessub4.ddl "-d 4d -s 0,0,1,2 -c 2,2,3,2 -"""S""" 1,1,3,3 -k 1,1,2,2 taindices.h5"
$
$ ! Exceed the dimensions for subsetting
$ CALL TOOLTEST texceedsubstart.ddl "-d 1d -s 1,3 taindices.h5"
$ CALL TOOLTEST texceedsubcount.ddl "-d 1d -c 1,3 taindices.h5"
$ CALL TOOLTEST texceedsubstride.ddl "-d 1d -"""S""" 1,3 taindices.h5"
$ CALL TOOLTEST texceedsubblock.ddl "-d 1d -k 1,3 taindices.h5"

$ ! Tests for filters
$ ! SZIP
$ CALL TOOLTEST tszip.ddl "-"""H""" -p -d szip tfilters.h5"
$ ! deflate
$ CALL TOOLTEST tdeflate.ddl "-"""H""" -p -d deflate tfilters.h5"
$ ! shuffle
$ CALL TOOLTEST tshuffle.ddl "-"""H""" -p -d shuffle tfilters.h5"
$ ! fletcher32
$ CALL TOOLTEST tfletcher32.ddl "-"""H""" -p -d fletcher32 tfilters.h5"
$ ! nbit
$ CALL TOOLTEST tnbit.ddl "-"""H""" -p -d nbit tfilters.h5"
$ ! scaleoffset
$ CALL TOOLTEST tscaleoffset.ddl "-"""H""" -p -d scaleoffset tfilters.h5"
$ ! all
$ CALL TOOLTEST tallfilters.ddl "-"""H""" -p -d all tfilters.h5"
$ ! User defined
$ CALL TOOLTEST tuserfilter.ddl "-"""H"""  -p -d myfilter tfilters.h5"

$ ! Test for displaying objects with very long names. The error message says:
$ ! %DIFF-F-READERR, error reading DISK$USER:[HDFGROUP.1_8_VMS_9_30.TOOLS.TESTFILES]TLONGLINKS.DDL;1
$ ! -RMS-W-RTB, 66573 byte record too large for user's buffer
$ ! %RMS-E-EOF, end of file detected 
$! CALL TOOLTEST tlonglinks.ddl "tlonglinks.h5"
$ ! Dimensions over 4GB, print boundary
$ CALL TOOLTEST tbigdims.ddl "-d dset4gb -s 4294967284 -c 22 tbigdims.h5"
$ ! Hyperslab read
$ CALL TOOLTEST thyperslab.ddl "thyperslab.h5"
$ ! Test for displaying dataset and attribute of null space
$ CALL TOOLTEST tnullspace.ddl "tnullspace.h5"
$
$ ! Test for displaying dataset and attribute of space with 0 dimension size
$ CALL TOOLTEST zerodim.ddl "zerodim.h5"
$
$ ! Test for long double (some systems do not have long double)
$ ! CALL TOOLTEST tldouble.ddl "tldouble.h5"
$
$ ! Test for vms
$ CALL TOOLTEST tvms.ddl "tvms.h5"
$
$ !test for binary output
$ CALL TOOLTEST tbin1.ddl "-d integer -o out1.bin -b """LE""" tbinary.h5"
$ CALL TOOLTEST tbin2.ddl "-d float   -o out2.bin -b """BE""" tbinary.h5"
$ CALL TOOLTEST tbin4.ddl "-d double   -o out4.bin -b """FILE""" tbinary.h5"
$
$ ! Test for string binary output
$ ! These 2 cases need new function to handle them
$ ! CALL TOOLTEST tstr2bin2.exp "-d /g2/dset2 -b -o tstr2bin2.txt tstr2.h5"
$ ! CALL TOOLTEST tstr2bin6.exp "-d /g6/dset6 -b -o tstr2bin6.txt tstr2.h5"

$ ! Test for dataset region references
$ CALL TOOLTEST tdatareg.ddl "tdatareg.h5"
$ CALL TOOLTEST tdataregR.ddl "-"""R""" tdatareg.h5"
$ CALL TOOLTEST tattrreg.ddl "tattrreg.h5"
$ CALL TOOLTEST tattrregR.ddl "-"""R""" tattrreg.h5"
$ ! commented out because I don't know how to do "Dataset1" in command line.
$ ! CALL TOOLTEST tbinregR.exp "-d /Dataset1 -s 0 -"""R""" -y -o tbinregR.txt tdatareg.h5"
$
$ ! tests for group creation order "1" tracked, "2" name, root tracked
$ CALL TOOLTEST tordergr1.ddl "--group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5"
$ CALL TOOLTEST tordergr2.ddl "--group=1 --sort_by=creation_order --sort_order=descending tordergr.h5"
$ CALL TOOLTEST tordergr3.ddl "-g 2 -q name -z ascending tordergr.h5"
$ CALL TOOLTEST tordergr4.ddl "-g 2 -q name -z descending tordergr.h5"
$ CALL TOOLTEST tordergr5.ddl "-q creation_order tordergr.h5"
$
$ ! Tests for attribute order
$ CALL TOOLTEST torderattr1.ddl "-"""H""" --sort_by=name --sort_order=ascending torderattr.h5"
$ CALL TOOLTEST torderattr2.ddl "-"""H""" --sort_by=name --sort_order=descending torderattr.h5"
$ CALL TOOLTEST torderattr3.ddl "-"""H""" --sort_by=creation_order --sort_order=ascending torderattr.h5"
$ CALL TOOLTEST torderattr4.ddl "-"""H""" --sort_by=creation_order --sort_order=descending torderattr.h5"
$
$ ! Tests for link references and order
$ CALL TOOLTEST torderlinks1.ddl "--sort_by=name --sort_order=ascending tfcontents1.h5"
$ CALL TOOLTEST torderlinks2.ddl "--sort_by=name --sort_order=descending tfcontents1.h5"

$ ! Test for floating point user defined printf format
$ CALL TOOLTEST tfpformat.ddl "-m %.7f tfpformat.h5"
$ ! Test for traversal of external links
$ CALL TOOLTEST textlinksrc.ddl "textlinksrc.h5"
$ CALL TOOLTEST textlinkfar.ddl "textlinkfar.h5"
$ ! Test for danglng external links 
$ CALL TOOLTEST textlink.ddl "textlink.h5"
$ ! Test for error stack display (BZ2048)
$ ! Commented out due to the difference of printing format
$ ! CALL TOOLTEST filter_fail.ddl "--enable-error-stack filter_fail.h5"
$ ! Test for -o -y for dataset with attributes
$ CALL TOOLTEST tall-6.ddl "-y -o data -d /g1/g1.1/dset1.1.1 tall.h5"
$
$ ! Test for dataset packed bits
$ ! Limits:
$ ! Maximum number of packed bits is 8 (for now).
$ ! Maximum integer size is 8 (for now).
$ ! Maximun Offset is 7 (Maximum size - 1).
$ ! Maximum Offset+Length is 8 (Maximum size).
$ ! Test Normal operation on both signed and unsigned int datasets.
$ ! Their rawdata output should be the same.
$ CALL TOOLTEST tpbitsSignedWhole.ddl "-d /"""DS08BITS""" -"""M""" 0,8 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedWhole.ddl "-d /"""DU08BITS""" -"""M""" 0,8 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedIntWhole.ddl "-d /"""DS16BITS""" -"""M""" 0,16 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedIntWhole.ddl "-d /"""DU16BITS""" -"""M""" 0,16 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongWhole.ddl "-d /"""DS32BITS""" -"""M""" 0,32 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongWhole.ddl "-d /"""DU32BITS""" -"""M""" 0,32 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongLongWhole.ddl "-d /"""DS64BITS""" -"""M""" 0,64 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongLongWhole.ddl "-d /"""DU64BITS""" -"""M""" 0,64 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongLongWhole63.ddl "-d /"""DS64BITS""" -"""M""" 0,63 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongLongWhole63.ddl "-d /"""DU64BITS""" -"""M""" 0,63 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongLongWhole1.ddl "-d /"""DS64BITS""" -"""M""" 1,63 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongLongWhole1.ddl "-d /"""DU64BITS""" -"""M""" 1,63 packedbits.h5"
$ ! Half sections
$ CALL TOOLTEST tpbitsSigned4.ddl "-d /"""DS08BITS""" -"""M""" 0,4,4,4 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsigned4.ddl "-d /"""DU08BITS""" -"""M""" 0,4,4,4 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedInt8.ddl "-d /"""DS16BITS""" -"""M""" 0,8,8,8 packedbits.h5" 
$ CALL TOOLTEST tpbitsUnsignedInt8.ddl "-d /"""DU16BITS""" -"""M""" 0,8,8,8 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLong16.ddl "-d /"""DS32BITS""" -"""M""" 0,16,16,16 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLong16.ddl "-d /"""DU32BITS""" -"""M""" 0,16,16,16 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongLong32.ddl "-d /"""DS64BITS""" -"""M""" 0,32,32,32 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongLong32.ddl "-d /"""DU64BITS""" -"""M""" 0,32,32,32 packedbits.h5"
$ ! Quarter sections 
$ CALL TOOLTEST tpbitsSigned2.ddl "-d /"""DS08BITS""" -"""M""" 0,2,2,2,4,2,6,2 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsigned2.ddl "-d /"""DU08BITS""" -"""M""" 0,2,2,2,4,2,6,2 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedInt4.ddl "-d /"""DS16BITS""" -"""M""" 0,4,4,4,8,4,12,4 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedInt4.ddl "-d /"""DU16BITS""" -"""M""" 0,4,4,4,8,4,12,4 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLong8.ddl "-d /"""DS32BITS""" -"""M""" 0,8,8,8,16,8,24,8 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLong8.ddl "-d /"""DU32BITS""" -"""M""" 0,8,8,8,16,8,24,8 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongLong16.ddl "-d /"""DS64BITS""" -"""M""" 0,16,16,16,32,16,48,16 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongLong16.ddl "-d /"""DU64BITS""" -"""M""" 0,16,16,16,32,16,48,16 packedbits.h5"
$ ! Begin and end
$ CALL TOOLTEST tpbitsSigned.ddl "-d /"""DS08BITS""" -"""M""" 0,2,2,6 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsigned.ddl "-d /"""DU08BITS""" -"""M""" 0,2,2,6 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedInt.ddl "-d /"""DS16BITS""" -"""M""" 0,2,10,6 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedInt.ddl "-d /"""DU16BITS""" -"""M""" 0,2,10,6 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLong.ddl "-d /"""DS32BITS""" -"""M""" 0,2,26,6 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLong.ddl "-d /"""DU32BITS""" -"""M""" 0,2,26,6 packedbits.h5"
$ CALL TOOLTEST tpbitsSignedLongLong.ddl "-d /"""DS64BITS""" -"""M""" 0,2,58,6 packedbits.h5"
$ CALL TOOLTEST tpbitsUnsignedLongLong.ddl "-d /"""DU64BITS""" -"""M""" 0,2,58,6 packedbits.h5"
$ ! Overlapped packed bits.
$ CALL TOOLTEST tpbitsOverlapped.ddl "-d /"""DS08BITS""" -"""M""" 0,1,1,1,2,1,0,3 packedbits.h5"
$ ! Maximum number of packed bits.
$ CALL TOOLTEST tpbitsMax.ddl "-d /"""DS08BITS""" -"""M""" 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5"
$ ! Compound type.
$ CALL TOOLTEST tpbitsCompound.ddl "-d /dset1 -"""M""" 0,1,1,1 tcompound.h5"
$ ! Array type.
$ CALL TOOLTEST tpbitsArray.ddl "-d /"""D"""ataset1 -"""M""" 0,1,1,1 tarray1.h5"
$ ! Test Error handling.
$ ! Too many packed bits requested. Max is 8 for now.
$ CALL TOOLTEST tpbitsMaxExceeded.ddl "-d /"""DS08BITS""" -"""M""" 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5"
$ ! Offset too large. Max is 7 (8-1) for now.
$ CALL TOOLTEST tpbitsOffsetExceeded.ddl "-d /"""DS08BITS""" -"""M""" 64,1 packedbits.h5"
$ ! Commented out due to the difference of printing format.
$ ! CALL TOOLTEST tpbitsCharOffsetExceeded.ddl "-d /"""DS08BITS""" -"""M""" 8,1 packedbits.h5"
$ ! CALL TOOLTEST tpbitsIntOffsetExceeded.ddl "-d /"""DS16BITS""" -"""M""" 16,1 packedbits.h5"
$ ! CALL TOOLTEST tpbitsLongOffsetExceeded.ddl "-d /"""DS32BITS""" -"""M""" 32,1 packedbits.h5"
$ ! Bad offset, must not be negative.
$ CALL TOOLTEST tpbitsOffsetNegative.ddl "-d /"""DS08BITS""" -"""M""" -1,1 packedbits.h5"
$ ! Bad length, must not be positive.
$ CALL TOOLTEST tpbitsLengthPositive.ddl "-d /"""DS08BITS""" -"""M""" 4,0 packedbits.h5"
$ ! Offset+Length is too large. Max is 8 for now.
$ CALL TOOLTEST tpbitsLengthExceeded.ddl "-d /"""DS08BITS""" -"""M""" 37,28 packedbits.h5"
$ ! Commented out due to the difference of printing format.
$ ! CALL TOOLTEST tpbitsCharLengthExceeded.ddl "-d /"""DS08BITS""" -"""M""" 2,7 packedbits.h5"
$ ! CALL TOOLTEST tpbitsIntLengthExceeded.ddl "-d /"""DS16BITS""" -"""M""" 10,7 packedbits.h5"
$ ! CALL TOOLTEST tpbitsLongLengthExceeded.ddl "-d /"""DS32BITS""" -"""M""" 26,7 packedbits.h5"
$ ! Incomplete pair of packed bits request.
$ CALL TOOLTEST tpbitsIncomplete.ddl "-d /"""DS08BITS""" -"""M""" 0,2,2,1,0,2,2, packedbits.h5"
$
$ !
$TOOLTEST: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "h5dumpout"
$ actual_err = base + "h5dumperr"
$
$ begin = "Testing h5dump "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ 
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$! write  sys$output "#############################"
$! write  sys$output "Expected output for 'h5dump ''P2''"
$! write  sys$output "#############################"
$ ON ERROR THEN CONTINUE
$ h5dump 'P2
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
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual' 'P1'
$ open/read temp_out h5dump_temp.dif
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
$ append/new_version h5dump_temp.dif h5dump.log
$ append/new_version 'actual'        h5dump_output.txt
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_err) .NES. ""
$ then
$  del *.h5dumperr;*
$ endif
$  del *.h5dumpout;*
$  del h5dump_temp.dif;*
$ !
$ENDSUBROUTINE
$
$TOOLTEST1: SUBROUTINE
$
$ len =  F$LENGTH(P1)
$ base = F$EXTRACT(0,len-3,P1)
$ actual = base + "h5dumpout"
$ actual_err = base + "h5dumperr"
$
$ begin = "Testing h5dump "
$ !
$ ! Run the test and save output in the 'actual' file
$ !
$ define/nolog sys$output 'actual'
$ define/nolog sys$error  'actual_err'
$ ON ERROR THEN CONTINUE
$ h5dump 'P2
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
$ diff/output=h5dump_temp/ignore=(spacing,trailing_spaces,blank_lines) 'actual' 'P1'
$ open/read temp_out h5dump_temp.dif
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
$ append/new_version h5dump_temp.dif h5dump.log
$ append/new_version 'actual'        h5dump_output.txt
$ !
$ ! Delete temporary files
$ !
$ if F$SEARCH(actual_err) .NES. ""
$ then
$  del *.h5dumperr;*
$ endif
$  del *.h5dumpout;*
$  del h5dump_temp.dif;*
$ !
$ENDSUBROUTINE

