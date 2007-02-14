#! /bin/sh
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
# Tests for the h5copy tool
#
# Pedro Vicente Nunes (pvn@hdfgroup.org), Albert Cheng (acheng@hdfgroup.org)
# Thursday, July 20, 2006
#

H5COPY=h5copy               # The tool name
H5COPY_BIN=`pwd`/$H5COPY    # The path of the tool binary
H5DIFF=h5diff               # The h5diff tool name 
H5DIFF_BIN=`pwd`/../h5diff/$H5DIFF    # The path of the h5diff tool binary
H5LS=h5ls                   # The h5ls tool name 
H5LS_ARGS=-vr               # Arguments to the h5ls tool
H5LS_BIN=`pwd`/../h5ls/$H5LS # The path of the h5ls tool binary

nerrors=0
verbose=yes

SRCFILE=h5copytst.h5
INDIR=$srcdir/../testfiles
OUTDIR=../testfiles
CMP='cmp -s'
DIFF='diff -c'

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi
test -d $OUTDIR || mkdir $OUTDIR

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() 
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY() 
{
    SPACES="                                                               "
    echo "Verifying h5diff output $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY_H5LS() 
{
    SPACES="                                                               "
    echo "Verifying h5ls file structure $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*. If h5copy can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable.
# Assumed arguments:
# $1 is -i
# $2 is input file
# $3 is -o
# $4 is output file
# $* everything else arguments for h5copy.

TOOLTEST() 
{
     runh5diff=yes
     if [ "$1" = -i ]; then
      inputfile=$2
     else
      runh5diff=no
     fi
     if [ "$3" = -o ]; then
      outputfile=$4
     else 
      runh5diff=no
     fi
  
    TESTING $H5COPY $@
    (
    echo "#############################"
    echo " output for '$H5COPY $@'"
    echo "#############################"
    $RUNSERIAL $H5COPY_BIN $@
    ) > output.out
    RET=$?
    if [ $RET != 0 ]; then
        echo "*FAILED*"
        echo "failed result is:"
        cat output.out
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"
    fi
    
    if [ $runh5diff != no ]; then
     H5DIFFTEST $inputfile $outputfile 
    fi
}

# Call the h5diff tool
#
H5DIFFTEST() 
{
    VERIFY  $@
    if [ "`uname -s`" = "TFLOPS O/S" ]; then
        $RUNSERIAL $H5DIFF_BIN $@ -q
    else
        $RUNSERIAL $H5DIFF_BIN "$@" -q
    fi
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi
}

# Call the h5ls tool to verify the correct output data in the destination file
#
H5LSTEST() 
{
    expect="$INDIR/`basename $1 .h5`.ls"
    actual="$OUTDIR/`basename $1 .h5`.out"

    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    VERIFY_H5LS  $@
    (
      echo "#############################"
      echo "Expected output for '$H5LS $@'" 
      echo "#############################"
      $RUNSERIAL $H5LS_BIN $H5LS_ARGS $@
    ) 2>&1 >$actual


   if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
    echo " CREATED"
    cp $actual $expect
   elif $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.ls) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi
}

# Copy single datasets of various forms from one group to another,
#       adding object copied to the destination file each time
#
# Assumed arguments:
# $1 is test "variation" (a single letter, normally)
# $2 is group within source file
# $3 is group within destination file
COPYOBJECTS() 
{
    TESTFILE="$INDIR/$SRCFILE"
    FILEOUT="$OUTDIR/`basename $SRCFILE .h5`.$1.out.h5"

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    # Test copying various forms of datasets
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"simple     -d "$3"simple
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"chunk      -d "$3"chunk
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"compact    -d "$3"compact
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"compound   -d "$3"compound
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"compressed -d "$3"compressed
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"named_vl   -d "$3"named_vl
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"nested_vl  -d "$3"nested_vl

    # Test copying & renaming dataset
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"compound   -d "$3"rename

    # Test copying empty & "full" groups
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"grp_empty  -d "$3"grp_empty
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"grp_dsets  -d "$3"grp_dsets

    # Test copying & renaming group
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s "$2"grp_dsets  -d "$3"grp_rename

    # Verify that the file created above is correct
    H5LSTEST $FILEOUT

    # Remove output file created, if the "no cleanup" environment variable is
    #   not defined
    if test -z "$HDF5_NOCLEANUP"; then
        rm -f $FILEOUT
    fi
}

##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################

echo "Copy objects from root group of source file to root of destination file"
echo "(with implicit root group paths)"
COPYOBJECTS a "" ""

echo "Copy objects from root group of source file to root of destination file"
echo "(with explicit root group paths)"
COPYOBJECTS b "/" "/"


if test $nerrors -eq 0 ; then
    echo "All h5copy tests passed."
fi

exit $nerrors

