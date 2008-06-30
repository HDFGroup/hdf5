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
H5LS_ARGS=-Svr              # Arguments to the h5ls tool
H5LS_BIN=`pwd`/../h5ls/$H5LS # The path of the h5ls tool binary

nerrors=0
verbose=yes

SRCFILE=h5copytst.h5
INDIR=$srcdir/testfiles
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

        # Clean up output file
        if test -z "$HDF5_NOCLEANUP"; then
           rm -f output.out
        fi
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
        $RUNSERIAL $H5DIFF_BIN -q $@ 
    else
        $RUNSERIAL $H5DIFF_BIN -q "$@" 
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
    #
    # Note:  The modification time and storage utilization are masked off
    #   so that the output is more portable
    VERIFY_H5LS  $@
    (
      echo "#############################"
      echo "Expected output for '$H5LS $@'" 
      echo "#############################"
      $RUNSERIAL $H5LS_BIN $H5LS_ARGS $@
    ) 2>&1 |sed 's/Modified:.*/Modified:  XXXX-XX-XX XX:XX:XX XXX/' |sed 's/Storage:.*/Storage:   <details removed for portability>/' >$actual


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

   # Clean up output file
   if test -z "$HDF5_NOCLEANUP"; then
      rm -f $actual $actual_err
   fi
}

# Copy single datasets of various forms from one group to another,
#       adding object copied to the destination file each time
#
# Assumed arguments:
# <none>
COPYOBJECTS() 
{
    TESTFILE="$INDIR/$SRCFILE"
    FILEOUT="$OUTDIR/`basename $SRCFILE .h5`.out.h5"

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    echo "Test copying various forms of datasets"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s simple     -d simple
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s chunk      -d chunk
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s compact    -d compact
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s compound   -d compound
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s compressed -d compressed
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s named_vl   -d named_vl
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s nested_vl  -d nested_vl

    echo "Test copying dataset within group in source file to root of destination"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s grp_dsets/simple  -d simple_top

    echo "Test copying & renaming dataset"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s compound   -d rename

    echo "Test copying empty, 'full' & 'nested' groups"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s grp_empty  -d grp_empty
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s grp_dsets  -d grp_dsets
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s grp_nested -d grp_nested

    echo "Test copying dataset within group in source file to group in destination"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s /grp_dsets/simple  -d /grp_dsets/simple_group

    echo "Test copying & renaming group"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s grp_dsets  -d grp_rename

    echo "Test copying 'full' group hierarchy into group in destination file"
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s grp_dsets  -d /grp_rename/grp_dsets

    echo "Test copying objects into group hier. that doesn't exist yet in destination file"
    TOOLTEST -i $TESTFILE -o $FILEOUT -vp -s simple    -d /A/B1/simple
    TOOLTEST -i $TESTFILE -o $FILEOUT -vp -s simple    -d /A/B2/simple2
    TOOLTEST -i $TESTFILE -o $FILEOUT -vp -s /grp_dsets/simple    -d /C/D/simple
    TOOLTEST -i $TESTFILE -o $FILEOUT -vp -s /grp_dsets -d /E/F/grp_dsets
    TOOLTEST -i $TESTFILE -o $FILEOUT -vp -s /grp_nested -d /G/H/grp_nested

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

COPYOBJECTS 


if test $nerrors -eq 0 ; then
    echo "All h5copy tests passed."
fi

exit $nerrors

