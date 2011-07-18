#!/bin/sh
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
#
# HDF Utilities Test script
# Usage: h5importtestutil.sh [machine-type]

TESTNAME=h5import
EXIT_SUCCESS=0
EXIT_FAILURE=1

CP='cp'

# initialize errors variable
nerrors=0

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

# source dirs
SRC_TOOLS="$srcdir/.."
SRC_TOOLS_TESTFILES="$SRC_TOOLS/testfiles"

# testfiles source dirs for tools
SRC_H5LS_TESTFILES="$SRC_TOOLS_TESTFILES"
SRC_H5DUMP_TESTFILES="$SRC_TOOLS_TESTFILES"
SRC_H5DIFF_TESTFILES="$SRC_TOOLS/h5diff/testfiles"
SRC_H5COPY_TESTFILES="$SRC_TOOLS/h5copy/testfiles"
SRC_H5REPACK_TESTFILES="$SRC_TOOLS/h5repack/testfiles"
SRC_H5JAM_TESTFILES="$SRC_TOOLS/h5jam/testfiles"
SRC_H5STAT_TESTFILES="$SRC_TOOLS/h5stat/testfiles"
SRC_H5IMPORT_TESTFILES="$SRC_TOOLS/h5import/testfiles"

TESTDIR=./testfiles
test -d $TESTDIR || mkdir $TESTDIR

######################################################################
# test files
# --------------------------------------------------------------------
# All the test files copy from source directory to test directory
# NOTE: Keep this framework to add/remove test files.
#       Any test files from other tools can be used in this framework.
#       This list are also used for checking exist.
#       Comment '#' without space can be used.
# --------------------------------------------------------------------
LIST_HDF5_TEST_FILES="
$SRC_H5IMPORT_TESTFILES/binfp64.h5
$SRC_H5IMPORT_TESTFILES/binin8.h5
$SRC_H5IMPORT_TESTFILES/binin8w.h5
$SRC_H5IMPORT_TESTFILES/binin16.h5
$SRC_H5IMPORT_TESTFILES/binin32.h5
$SRC_H5IMPORT_TESTFILES/binuin16.h5
$SRC_H5IMPORT_TESTFILES/binuin32.h5
$SRC_H5IMPORT_TESTFILES/txtfp32.h5
$SRC_H5IMPORT_TESTFILES/txtfp64.h5
$SRC_H5IMPORT_TESTFILES/txtin8.h5
$SRC_H5IMPORT_TESTFILES/txtin16.h5
$SRC_H5IMPORT_TESTFILES/txtin32.h5
$SRC_H5IMPORT_TESTFILES/txtuin16.h5
$SRC_H5IMPORT_TESTFILES/txtuin32.h5
$SRC_H5IMPORT_TESTFILES/txtstr.h5
$SRC_H5IMPORT_TESTFILES/textpfe.h5
"

LIST_OTHER_TEST_FILES="
$SRC_H5IMPORT_TESTFILES/binfp64.conf
$SRC_H5IMPORT_TESTFILES/binin8.conf
$SRC_H5IMPORT_TESTFILES/binin8w.conf
$SRC_H5IMPORT_TESTFILES/binin16.conf
$SRC_H5IMPORT_TESTFILES/binin32.conf
$SRC_H5IMPORT_TESTFILES/binuin16.conf
$SRC_H5IMPORT_TESTFILES/binuin32.conf
$SRC_H5IMPORT_TESTFILES/txtfp32.conf
$SRC_H5IMPORT_TESTFILES/txtfp64.conf
$SRC_H5IMPORT_TESTFILES/txtin8.conf
$SRC_H5IMPORT_TESTFILES/txtin16.conf
$SRC_H5IMPORT_TESTFILES/txtin32.conf
$SRC_H5IMPORT_TESTFILES/txtuin16.conf
$SRC_H5IMPORT_TESTFILES/txtuin32.conf
$SRC_H5IMPORT_TESTFILES/textpfe.conf
$SRC_H5IMPORT_TESTFILES/txtstr.conf
$SRC_H5IMPORT_TESTFILES/txtfp32.txt
$SRC_H5IMPORT_TESTFILES/txtfp64.txt
$SRC_H5IMPORT_TESTFILES/txtuin32.txt
$SRC_H5IMPORT_TESTFILES/txtin16.txt
$SRC_H5IMPORT_TESTFILES/txtin32.txt
$SRC_H5IMPORT_TESTFILES/textpfe64.txt
$SRC_H5IMPORT_TESTFILES/txtstr.txt
"

#
# copy test files and expected output files from source dirs to test dir
#
COPY_TESTFILES="$LIST_HDF5_TEST_FILES $LIST_OTHER_TEST_FILES"

COPY_TESTFILES_TO_TESTDIR()
{
    # copy test files. Used -f to make sure get a new copy
    for tstfile in $COPY_TESTFILES
    do
        # ignore '#' comment
        echo $tstfile | tr -d ' ' | grep '^#' > /dev/null
        RET=$?
        if [ $RET -eq 1 ]; then
            if [ -a $tstfile ]; then
                $CP -f $tstfile $TESTDIR
            else
                echo "Error: FAILED to copy $tstfile"
                echo "       $tstfile doesn't exist!"
                exit $EXIT_FAILURE
            fi
        fi
    done
}

TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

TOOLTEST()
{
err=0
$RUNSERIAL ./h5import $*
$RUNSERIAL ../h5dump/h5dump $5 >log2

cd tmp_testfiles
$RUNSERIAL ../../h5dump/h5dump $5 >log1
cd ..

cmp -s tmp_testfiles/log1 log2 || err=1
rm -f log2 tmp_testfiles/log1
if [ $err -eq 1 ]; then
nerrors="` expr $nerrors + 1 `";
  echo "*FAILED*"
else
  echo " PASSED"
fi
}

echo "" 
echo "=============================="
echo "H5IMPORT tests started"
echo "=============================="

if [ -f h5import -a -f h5importtest ]; then
#echo "** Testing h5import  ***"

rm -f  output.h5 log1 tx* b* *.dat

# prepare for test
COPY_TESTFILES_TO_TESTDIR

mkdir tmp_testfiles
$CP $TESTDIR/*.h5 ./tmp_testfiles/

$RUNSERIAL ./h5importtest

################################################
###        T H E   T E S T S
################################################

TESTING "ASCII I32 rank 3 - Output BE " ;
TOOLTEST $TESTDIR/txtin16.txt -c $TESTDIR/txtin32.conf -o txtin32.h5

TESTING "ASCII I16 rank 3 - Output LE - CHUNKED - extended" 
TOOLTEST $TESTDIR/txtin16.txt -c $TESTDIR/txtin16.conf -o txtin16.h5


TESTING "ASCII I8 - rank 3 - Output I8 LE-Chunked+Extended+Compressed " 
TOOLTEST $TESTDIR/txtin16.txt -c $TESTDIR/txtin8.conf  -o txtin8.h5

TESTING "ASCII UI32 - rank 3 - Output BE" 
TOOLTEST $TESTDIR/txtuin32.txt -c $TESTDIR/txtuin32.conf -o txtuin32.h5

TESTING "ASCII UI16 - rank 2 - Output LE+Chunked+Compressed " 
TOOLTEST $TESTDIR/txtuin32.txt -c $TESTDIR/txtuin16.conf -o txtuin16.h5

TESTING "ASCII F32 - rank 3 - Output LE " 
TOOLTEST $TESTDIR/txtfp32.txt -c $TESTDIR/txtfp32.conf -o txtfp32.h5


TESTING "ASCII F64 - rank 3 - Output BE + CHUNKED+Extended+Compressed " 
TOOLTEST $TESTDIR/txtfp64.txt -c $TESTDIR/txtfp64.conf -o txtfp64.h5

TESTING "BINARY F64 - rank 3 - Output LE+CHUNKED+Extended+Compressed " 
TOOLTEST binfp64.bin -c $TESTDIR/binfp64.conf -o binfp64.h5


TESTING "BINARY I16 - rank 3 - Output order LE + CHUNKED + extended " 
TOOLTEST binin16.bin -c $TESTDIR/binin16.conf -o binin16.h5


TESTING "BINARY I8 - rank 3 - Output I16LE + Chunked+Extended+Compressed " 
TOOLTEST binin8.bin -c $TESTDIR/binin8.conf  -o binin8.h5


TESTING "BINARY I32 - rank 3 - Output BE + CHUNKED " 
TOOLTEST binin32.bin -c $TESTDIR/binin32.conf -o binin32.h5


TESTING "BINARY UI16 - rank 3 - Output byte BE + CHUNKED " 
TOOLTEST binuin16.bin -c $TESTDIR/binuin16.conf -o binuin16.h5

TESTING "BINARY UI32 - rank 3 - Output LE + CHUNKED " 
TOOLTEST binuin32.bin -c $TESTDIR/binuin32.conf -o binuin32.h5


TESTING "STR" 
TOOLTEST $TESTDIR/txtstr.txt -c $TESTDIR/txtstr.conf -o txtstr.h5

TESTING "BINARY I8 CR LF EOF" 
TOOLTEST binin8w.bin -c $TESTDIR/binin8w.conf -o binin8w.h5

TESTING "ASCII F64 - rank 1 - INPUT-CLASS TEXTFPE " 
TOOLTEST $TESTDIR/textpfe64.txt -c $TESTDIR/textpfe.conf -o textpfe.h5



rm -f  txtin32.txt txtin16.txt *.bin *.h5
rm -rf tmp_testfiles
else
  echo "** h5import or h5importtest not available ***"
  nerrors="` expr $nerrors + 1 `";
fi

#
# Check errors result
if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
