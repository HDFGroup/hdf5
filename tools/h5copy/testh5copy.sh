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

TESTNAME=h5copy
EXIT_SUCCESS=0
EXIT_FAILURE=1

######################################################################
# test files
# --------------------------------------------------------------------
# All the test files copy from source directory to test directory
# NOTE: Keep this framework to add/remove test files.
#       Any test files from other tools can be used in this framework.
#       This list are also used for checking exist.
#       Comment '#' without space can be used.
# --------------------------------------------------------------------
# List of files that will be copied over to local test dir
LIST_HDF5_TEST_FILES="
$SRC_H5COPY_TESTFILES/h5copytst.h5
$SRC_H5COPY_TESTFILES/h5copy_ref.h5
$SRC_H5COPY_TESTFILES/h5copy_extlinks_src.h5
$SRC_H5COPY_TESTFILES/h5copy_extlinks_trg.h5
"

# List of expect files that will be copied over to local test dir
LIST_OTHER_TEST_FILES="
$SRC_H5COPY_TESTFILES/h5copy_extlinks_src.out.ls
$SRC_H5COPY_TESTFILES/h5copy_ref.out.ls
$SRC_H5COPY_TESTFILES/h5copytst.out.ls
$SRC_H5COPY_TESTFILES/h5copy_misc1.out
"

H5COPY=h5copy               # The tool name
H5COPY_BIN=`pwd`/$H5COPY    # The path of the tool binary
H5DIFF=h5diff               # The h5diff tool name 
H5DIFF_BIN=`pwd`/../h5diff/$H5DIFF    # The path of the h5diff tool binary
H5LS=h5ls                   # The h5ls tool name 
H5LS_ARGS=-Svr              # Arguments to the h5ls tool
H5LS_BIN=`pwd`/../h5ls/$H5LS # The path of the h5ls tool binary
CMP='cmp -s'
DIFF='diff -c'
CP='cp'

nerrors=0
verbose=yes
h5haveexitcode=yes	    # default is yes

TESTDIR=./testfiles
test -d $TESTDIR || mkdir $TESTDIR

# RUNSERIAL is used. Check if it can return exit code from executalbe correctly.
if [ -n "$RUNSERIAL_NOEXITCODE" ]; then
    echo "***Warning*** Serial Exit Code is not passed back to shell corretly."
    echo "***Warning*** Exit code checking is skipped."
    h5haveexitcode=no
fi

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
                echo "Error: FAILED to copy $tstfile."
                echo "       $tstfile doesn't exist!"
                exit $EXIT_FAILURE
            fi
        fi
    done
}

# Print a "SKIP" message
SKIP() {
	 TESTING $H5COPY $@
	  echo  " -SKIP-"
}

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

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY_OUTPUT() 
{
    SPACES="                                                               "
    echo "Verifying output files $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Source in the output filter function definitions.
. $srcdir/../../bin/output_filter.sh

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
    actualout="$TESTDIR/tooltest.actualout"
    actualerr="$TESTDIR/tooltest.actualerr"
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
    ) > $actualout 2> $actualerr
    RET=$?
    if [ $RET != 0 ]; then
        echo "*FAILED*"
        echo "failed result is:"
        cat $actualout
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"

        # Clean up output file
        if test -z "$HDF5_NOCLEANUP"; then
           rm -f $actualout $actualerr
        fi
    fi
    
    if [ $runh5diff != no ]; then
     H5DIFFTEST $inputfile $outputfile $7 $9
    fi
}


# Compare the two text files
# PASS if same
# FAIL if different, and show the diff
#
# Assumed arguments:
# $1 is text file1 (expected output)
# $2 is text file2 (actual output)
CMP_OUTPUT()
{
    expect=$1
    actual=$2

    VERIFY_OUTPUT $@
    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
    elif $CMP $expect $actual; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected output differs from actual output"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
}

TOOLTEST_FAIL() 
{
    expectout="$TESTDIR/$1"
    actualout="$TESTDIR/$1.actualout"
    actualerr="$TESTDIR/$1.actualerr"
    actualout_sav=${actualout}-sav
    actualerr_sav=${actualerr}-sav
    shift
    if [ "$1" = -i ]; then
      inputfile=$2
    fi
    if [ "$3" = -o ]; then
      outputfile=$4
    fi

    TESTING $H5COPY $@
    (
    #echo "#############################"
    #echo " output for '$H5COPY $@'"
    #echo "#############################"
    $RUNSERIAL $H5COPY_BIN $@
    ) > $actualout 2> $actualerr

    RET=$?
    # save actualout and actualerr in case they are needed later.
    cp $actualout $actualout_sav
    STDOUT_FILTER $actualout
    cp $actualerr $actualerr_sav
    STDERR_FILTER $actualerr
    if [ $RET != 0 ]; then
        echo " PASSED"
        # Verifying output text from h5copy
        if [ "$expectout" != "SKIP" ]; then
            # combine stderr to stdout to compare the output at once.
            # We may seperate stdout and stderr later.
            cat $actualerr >> $actualout
            CMP_OUTPUT $expectout $actualout
        fi
    else
        echo "*FAILED*"
        echo "failed result is:"
        cat $actualout
        nerrors="`expr $nerrors + 1`"
    fi
   

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
       rm -f $actualout $actualerr $actualout_sav $actualerr_sav
    fi
}


# Call the h5diff tool
#
H5DIFFTEST() 
{
    VERIFY  $@
    $RUNSERIAL $H5DIFF_BIN -q "$@" 
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi
}

# Call the h5diff tool with a call that is expected to fail
#
H5DIFFTEST_FAIL() 
{
    VERIFY  $@
    $RUNSERIAL $H5DIFF_BIN -q "$@" 
    RET=$?

    if [ $h5haveexitcode = 'yes' -a $RET != 1 ] ; then
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
    expect="$TESTDIR/`basename $1 .h5`.ls"
    actual="$TESTDIR/`basename $1 .h5`.ls.actualout"

    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    #
    # Note:  The modification time and storage utilization are masked off
    #   so that the output is more portable
    VERIFY_H5LS  $@
    (
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
COPY_OBJECTS() 
{
    TESTFILE="$TESTDIR/h5copytst.h5"
    FILEOUT="$TESTDIR/`basename h5copytst.h5 .h5`.out.h5"

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    echo "Testing from `basename $TESTFILE` to `basename $FILEOUT` for the following tests:"
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

# Copy references in various way.
# adding to the destination file each time compare the result
#
# Assumed arguments:
# <none>
COPY_REFERENCES() 
{
    TESTFILE="$TESTDIR/h5copy_ref.h5"
    FILEOUT="$TESTDIR/`basename h5copy_ref.h5 .h5`.out.h5"

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    echo "Test copying object and region references"
    TOOLTEST -f ref -i $TESTFILE -o $FILEOUT -v -s / -d /COPY

    # Verify that the file created above is correct
    H5LSTEST $FILEOUT

    # Remove output file created, if the "no cleanup" environment variable is
    #   not defined
    if test -z "$HDF5_NOCLEANUP"; then
        rm -f $FILEOUT
    fi
}

# Copy external links.
# adding to the destination file each time compare the result
#
# Assumed arguments:
# <none>
COPY_EXT_LINKS() 
{
    TESTFILE="$TESTDIR/h5copy_extlinks_src.h5"
    FILEOUT="$TESTDIR/`basename h5copy_extlinks_src.h5 .h5`.out.h5"

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    echo "Test copying external link directly without -f ext"
    TOOLTEST -v -i $TESTFILE -o $FILEOUT -s /group_ext/extlink_dset -d /copy1_dset

    echo "Test copying external link directly with -f ext"
    TOOLTEST -f ext -i $TESTFILE -o $FILEOUT -v -s /group_ext/extlink_dset -d /copy2_dset

    echo "Test copying dangling external link (no obj) directly without -f ext"
    TOOLTEST -v -i $TESTFILE -o $FILEOUT -s /group_ext/extlink_notyet1 -d /copy_dangle1_1

    echo "Test copying dangling external link (no obj) directly with -f ext"
    TOOLTEST -f ext -i $TESTFILE -o $FILEOUT -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_2

    echo "Test copying dangling external link (no file) directly without -f ext"
    TOOLTEST -v -i $TESTFILE -o $FILEOUT -s /group_ext/extlink_notyet2 -d /copy_dangle2_1

    echo "Test copying dangling external link (no file) directly with -f ext"
    TOOLTEST -f ext -i $TESTFILE -o $FILEOUT -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_2

    echo "Test copying a group contains external links without -f ext"
    TOOLTEST -v -i $TESTFILE -o $FILEOUT -s /group_ext -d /copy1_group

    echo "Test copying a group contains external links with -f ext"
    TOOLTEST -f ext -v -i $TESTFILE -o $FILEOUT -s /group_ext -d /copy2_group

    # Verify that the file created above is correct
    H5LSTEST $FILEOUT

    # Remove output file created, if the "no cleanup" environment variable is
    #   not defined
    if test -z "$HDF5_NOCLEANUP"; then
        rm -f $FILEOUT
    fi
}

# Test misc.
#
# Assumed arguments:
# <none>
TEST_MISC() 
{
    TESTFILE="$TESTDIR/h5copytst.h5"
    FILEOUT="$TESTDIR/`basename h5copytst.h5 .h5`.out.h5"

    # Remove any output file left over from previous test run
    rm -f $FILEOUT

    echo "Test copying object into group which doesn't exist, without -p"
    TOOLTEST_FAIL h5copy_misc1.out -v -i $TESTFILE -o $FILEOUT -s /simple  -d /g1/g2/simple

    echo "Test copying objects to the same file "
    rm -f $FILEOUT
    # create temporary test file ($FILEOUT) with some objects
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s /simple -d /simple 
    TOOLTEST -i $TESTFILE -o $FILEOUT -v -s /grp_dsets  -d /grp_dsets
    # actual test cases
    TOOLTEST -i $FILEOUT -o $FILEOUT -v -s /simple -d /simple_cp
    TOOLTEST -i $FILEOUT -o $FILEOUT -v -s /grp_dsets  -d /grp_dsets_cp

    # Remove output file created, if the "no cleanup" environment variable is
    #   not defined
    if test -z "$HDF5_NOCLEANUP"; then
        rm -f $FILEOUT
    fi
}

##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
# prepare for test
COPY_TESTFILES_TO_TESTDIR

# Start tests
COPY_OBJECTS 
COPY_REFERENCES
COPY_EXT_LINKS
TEST_MISC


if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
