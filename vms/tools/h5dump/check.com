$!#
$!# Copyright by the Board of Trustees of the University of Illinois.
$!# All rights reserved.
$!#
$!# This file is part of HDF5.  The full HDF5 copyright notice, including
$!# terms governing use, modification, and redistribution, is contained in
$!# the files COPYING and Copyright.html.  COPYING can be found at the root
$!# of the source code distribution tree; Copyright.html can be found at the
$!# root level of an installed copy of the electronic HDF5 document set and
$!# is linked from the top-level documents page.  It can also be found at
$!# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
$!# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
$!#
$!
$! h5dump testing script
$!
$ define sys$output h5dump.out
$ define sys$error  h5dump.err
$ h5dump :== $sys$sysusers:[pourmale.hdf5.tools.h5dump]h5dump.exe
$! test for displaying groups 

$! h5dump  tgroup.h5
$! test for displaying the selected groups
$! h5dump --group=/g2 --group / -g /y tgroup.h5

$! test for displaying simple space datasets
$! h5dump tdset.h5
$ type sys$input
      ----- test for displaying selected datasets 
$ h5dump  "-H" -d dset1 -d /dset2 --dataset=dset3 tdset.h5
$! h5dump  "-H" -d dset1 -d /dset2 -d dset3 tdset.h5
$
$! test for displaying attributes
$! h5dump  tattr.h5
$! test for displaying the selected attributes of string type and scalar space
$ h5dump  -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5
$! test for header and error messages
$ type sys$input
      ----- test for displaying shared datatype in attribute
$ h5dump  "-A" tnamed_dtype_attr.h5

$! test for displaying soft links
$! h5dump  tslink.h5
$! test for displaying the selected link
$! h5dump -l slink2 tslink.h5

$! tests for hard links
$! h5dump thlink.h5
$! h5dump -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5
$! h5dump -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5
$! h5dump -g /g1 thlink.h5
$! h5dump -d /dset1 -g /g2 -d /g1/dset2 thlink.h5

$! tests for compound data types
$! h5dump tcompound.h5
$! test for named data types
$! h5dump -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5
$! test for unamed type 

$! test complicated compound datatype
$! h5dump tcompound_complex.h5

$!test for the nested compound type
$! h5dump tnestedcomp.h5

$! test for options
$! h5dump tall.h5
$! h5dump --header -g /g1/g1.1 -a attr2 tall.h5
$! h5dump -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5

$! test for loop detection
$! h5dump tloop.h5

$! test for string 
$! h5dump tstr.h5
$! h5dump tstr2.h5

$! test for file created by Lib SAF team
$! h5dump tsaf.h5

$! test for file with variable length data
$! h5dump tvldtypes1.h5
$! h5dump tvldtypes2.h5
$! h5dump tvldtypes3.h5
$! h5dump tvldtypes4.h5
$! h5dump tvldtypes5.h5

$!test for file with variable length string data
$! h5dump tvlstr.h5

$! test for files with array data
$! h5dump tarray1.h5
$! h5dump tarray2.h5
$! h5dump tarray3.h5
$! h5dump tarray4.h5
$! h5dump tarray5.h5
$! h5dump tarray6.h5
$! h5dump tarray7.h5

$! test for files with empty data
$! h5dump tempty.h5

$! test for files with groups that have comments
$! h5dump tgrp_comments.h5

$! test the --filedriver flag
$! h5dump  --filedriver=split tsplit_file
$! h5dump  --filedriver=family tfamily%05d.h5
$! h5dump  --filedriver=multi tmulti

$! test for files with group names which reach > 1024 bytes in size
$! h5dump  -w157 tlarge_objname.h5
$ type sys$input
       ----- test '-A' to suppress data but print attr's
$ h5dump  "-A" tall.h5

$! test '-r' to print attributes in ASCII instead of decimal
$ type sys$input
       ----- test -r to print in ASCII instead of decimal
$ h5dump  "-A" -r tall.h5

$! test Subsetting
$! h5dump --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5
$! h5dump -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5
$! h5dump -d "/dset1[1,1;;;]" tdset.h5
$! h5dump -d "/dset1[;3,2;4,4;1,4]" tdset2.h5

$! test printing characters in ASCII instead of decimal
$! h5dump -r tchar.h5

$! test failure handling
$! Missing file name
$! h5dump 

$! rev. 2004
$ type sys$input
       ------ test for super block
$ h5dump "-H" "-B" -d dset tfcontents1.h5
$ type sys$input
       ------ test for super block
$ h5dump "-B" tfcontents2.h5

$! test -p with a non existing dataset
$! h5dump -p -d bogus tfcontents1.h5

$! test for file contents
$! h5dump -n tfcontents1.h5

$! tests for storage layout
$! compact
$! h5dump --header -p -d compact tfilters.h5
$! contiguous
$! h5dump --header -p -d contiguous tfilters.h5
$! chunked
$! h5dump --header -p -d chunked tfilters.h5
$! external 
$! h5dump --header -p -d external tfilters.h5

$! fill values
$! h5dump -p tfvalues.h5

$! several datatype, with references , print path
$! h5dump  tattr2.h5

$! escape/not escape non printable characters
$! h5dump -e tstr3.h5
$! h5dump tstr3.h5
$! char data as ASCII with non escape
$! h5dump  -r -d str4 tstr3.h5

$! array indices print/not print
$! h5dump taindices.h5
$! h5dump -y taindices.h5


$! tests for filters
$! SZIP
$!option="-H -p -d szip tfilters.h5"
$!if test $USE_FILTER_SZIP != "yes"; then
$! SKIP $option
$!else
$!$! h5dump tszip.ddl $option
$!fi
$$! deflate
$!option="-H -p -d deflate tfilters.h5"
$!if test $USE_FILTER_DEFLATE != "yes"; then
$! SKIP $option
$!else
$! $! h5dump tdeflate.ddl $option
$!fi
$!$! shuffle
$!option="-H -p -d shuffle tfilters.h5"
$!if test $USE_FILTER_SHUFFLE != "yes"; then
$! SKIP $option
$!else
$! $! h5dump tshuffle.ddl $option
$!fi
$! fletcher32
$!option="-H -p -d fletcher32  tfilters.h5"
$!if test $USE_FILTER_FLETCHER32 != "yes"; then
$! SKIP $option
$!else
$! $! h5dump tfletcher32.ddl $option
$!fi
$! nbit
$!option="-H -p -d nbit  tfilters.h5"
$!if test $USE_FILTER_NBIT != "yes"; then
$! SKIP $option
$!else
$! $! h5dump tnbit.ddl $option
$!fi
$!$! scaleoffset
$!option="-H -p -d scaleoffset  tfilters.h5"
$!if test $USE_FILTER_SCALEOFFSET != "yes"; then
$! SKIP $option
$!else
$! $! h5dump tscaleoffset.ddl $option
$!fi
$! all
$!option="-H -p -d all  tfilters.h5"
$!if test $USE_FILTER_FLETCHER32 != "yes" -o  $USE_FILTER_SZIP != "yes" -o  $USE_FILTER_DEFLATE != "yes" -o  $USE_FILTER_SHUFFLE != "yes" -o $USE_FILTER_NBIT != "yes" -o  $USE_FILTER_SCALEOFFSET != "yes"; then
$! SKIP $option
$!else
$! $! h5dump tallfilters.ddl $option
$!fi
$! user defined
$! h5dump tuserfilter.ddl -H  -p -d myfilter  tfilters.h5
    
$! test for displaying dataset and attribute of null space
$! h5dump tnullspace.h5

$! test for displaying objects with very long names
$! h5dump tlonglinks.h5


