#! /bin/sh
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
#
# Tests for the h5dump tool

DUMPER=h5dump               # The tool name
DUMPER_BIN=`pwd`/$DUMPER    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ../testfiles || mkdir ../testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
#
TOOLTEST() {
   expect="$srcdir/../testfiles/$1"
   actual="../testfiles/`basename $1 .ddl`.out"
   actual_err="../testfiles/`basename $1 .ddl`.err"
   shift

   # Run test.
   TESTING $DUMPER $@
   (
      echo "#############################"
      echo "Expected output for '$DUMPER $@'" 
      echo "#############################"
      cd $srcdir/../testfiles
      $RUNSERIAL $DUMPER_BIN $@
   ) >$actual 2>$actual_err
   cat $actual_err >> $actual
    
   if $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.ddl) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi

   # Clean up output file
   if test -z "$HDF5_NOCLEANUP"; then
      rm -f $actual $actual_err
   fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test for displaying groups
TOOLTEST tgroup-1.ddl tgroup.h5
# test for displaying the selected groups
TOOLTEST tgroup-2.ddl --group=/g2 --group / -g /y tgroup.h5

# test for displaying simple space datasets
TOOLTEST tdset-1.ddl tdset.h5
# test for displaying selected datasets
TOOLTEST tdset-2.ddl -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5

# test for displaying attributes
TOOLTEST tattr-1.ddl tattr.h5
# test for displaying the selected attributes of string type and scalar space
TOOLTEST tattr-2.ddl -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5
# test for header and error messages
TOOLTEST tattr-3.ddl --header -a /attr2 --attribute=/attr tattr.h5

# test for displaying soft links
TOOLTEST tslink-1.ddl tslink.h5
# test for displaying the selected link
TOOLTEST tslink-2.ddl -l slink2 tslink.h5

# tests for hard links
TOOLTEST thlink-1.ddl thlink.h5
TOOLTEST thlink-2.ddl -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5
TOOLTEST thlink-3.ddl -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5
TOOLTEST thlink-4.ddl -g /g1 thlink.h5
TOOLTEST thlink-5.ddl -d /dset1 -g /g2 -d /g1/dset2 thlink.h5

# tests for compound data types
TOOLTEST tcomp-1.ddl tcompound.h5
# test for named data types
TOOLTEST tcomp-2.ddl -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5
# test for unamed type 
TOOLTEST tcomp-3.ddl -t /#5992:0 -g /group2 tcompound.h5

#test for the nested compound type
TOOLTEST tnestcomp-1.ddl tnestedcomp.h5

# test for options
TOOLTEST tall-1.ddl tall.h5
TOOLTEST tall-2.ddl --header -g /g1/g1.1 -a attr2 tall.h5
TOOLTEST tall-3.ddl -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5

# test for loop detection
TOOLTEST tloop-1.ddl tloop.h5

# test for string 
TOOLTEST tstr-1.ddl tstr.h5
TOOLTEST tstr-2.ddl tstr2.h5

# test for file created by Lib SAF team
TOOLTEST tsaf.ddl tsaf.h5

# test for file with variable length data
TOOLTEST tvldtypes1.ddl tvldtypes1.h5
TOOLTEST tvldtypes2.ddl tvldtypes2.h5
TOOLTEST tvldtypes3.ddl tvldtypes3.h5
TOOLTEST tvldtypes4.ddl tvldtypes4.h5

#test for file with variable length string data
TOOLTEST tvlstr.ddl tvlstr.h5

# test for files with array data
TOOLTEST tarray1.ddl tarray1.h5
TOOLTEST tarray2.ddl tarray2.h5
TOOLTEST tarray3.ddl tarray3.h5
TOOLTEST tarray4.ddl tarray4.h5
TOOLTEST tarray5.ddl tarray5.h5
TOOLTEST tarray6.ddl tarray6.h5
TOOLTEST tarray7.ddl tarray7.h5

# test for files with empty data
TOOLTEST tempty.ddl tempty.h5

# test for files with groups that have comments
TOOLTEST tgrp_comments.ddl tgrp_comments.h5

# test the --filedriver flag
TOOLTEST tsplit_file.ddl --filedriver=split tsplit_file
TOOLTEST tfamily.ddl --filedriver=family tfamily%05d.h5
TOOLTEST tmulti.ddl --filedriver=multi tmulti

# test for files with group names which reach > 1024 bytes in size
TOOLTEST tlarge_objname.ddl -w157 tlarge_objname.h5

# test Subsetting
TOOLTEST tall-4s.ddl --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5
TOOLTEST tall-5s.ddl -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5
TOOLTEST tdset-3s.ddl -d "/dset1[1,1;;;]" tdset.h5
TOOLTEST tdset2-1s.ddl -d "/dset1[;3,2;4,4;1,4]" tdset2.h5

# test failure handling
# Missing file name
TOOLTEST tnofilename.ddl

# test XML
TOOLTEST tall.h5.xml --xml tall.h5
TOOLTEST tattr.h5.xml --xml tattr.h5
TOOLTEST tbitfields.h5.xml --xml tbitfields.h5
TOOLTEST tcompound.h5.xml --xml tcompound.h5
TOOLTEST tcompound2.h5.xml --xml tcompound2.h5
TOOLTEST tdatareg.h5.xml --xml tdatareg.h5
TOOLTEST tdset.h5.xml --xml tdset.h5
TOOLTEST tdset2.h5.xml --xml tdset2.h5
TOOLTEST tenum.h5.xml --xml tenum.h5
TOOLTEST tgroup.h5.xml --xml tgroup.h5
TOOLTEST thlink.h5.xml --xml thlink.h5
TOOLTEST tloop.h5.xml --xml tloop.h5
TOOLTEST tloop2.h5.xml --xml tloop2.h5
TOOLTEST tmany.h5.xml --xml tmany.h5
TOOLTEST tnestedcomp.h5.xml --xml tnestedcomp.h5
TOOLTEST tobjref.h5.xml --xml tobjref.h5
TOOLTEST topaque.h5.xml --xml topaque.h5
TOOLTEST tslink.h5.xml --xml tslink.h5
TOOLTEST tstr.h5.xml --xml tstr.h5
TOOLTEST tstr2.h5.xml --xml tstr2.h5
TOOLTEST tref.h5.xml --xml tref.h5
TOOLTEST tname-amp.h5.xml --xml tname-amp.h5
TOOLTEST tname-apos.h5.xml --xml tname-apos.h5
TOOLTEST tname-gt.h5.xml --xml tname-gt.h5
TOOLTEST tname-lt.h5.xml --xml tname-lt.h5
TOOLTEST tname-quot.h5.xml --xml tname-quot.h5
TOOLTEST tname-sp.h5.xml --xml tname-sp.h5
TOOLTEST tstring.h5.xml --xml tstring.h5
TOOLTEST tstring-at.h5.xml --xml tstring-at.h5
TOOLTEST tref-escapes.h5.xml --xml tref-escapes.h5
TOOLTEST tref-escapes-at.h5.xml --xml tref-escapes-at.h5
TOOLTEST tnodata.h5.xml --xml tnodata.h5
TOOLTEST tarray1.h5.xml --xml tarray1.h5
TOOLTEST tarray2.h5.xml --xml tarray2.h5
TOOLTEST tarray3.h5.xml --xml tarray3.h5
TOOLTEST tarray6.h5.xml --xml tarray6.h5
TOOLTEST tarray7.h5.xml --xml tarray7.h5
TOOLTEST tvldtypes1.h5.xml --xml tvldtypes1.h5
TOOLTEST tvldtypes2.h5.xml --xml tvldtypes2.h5
TOOLTEST tvldtypes3.h5.xml --xml tvldtypes3.h5
TOOLTEST tvlstr.h5.xml --xml tvlstr.h5
TOOLTEST tsaf.h5.xml --xml tsaf.h5
TOOLTEST tempty.h5.xml --xml tempty.h5

if test $nerrors -eq 0 ; then
   echo "All $DUMPER tests passed."
fi

exit $nerrors
