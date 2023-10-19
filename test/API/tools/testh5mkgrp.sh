#!/usr/bin/env bash
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# Tests for the h5mkgrp tool
#

srcdir=@srcdir@

TESTNAME=h5mkgrp
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5MKGRP=h5mkgrp             # The h5mkgrp tool name
H5LS=h5ls                   # The h5ls tool name
H5LS_ARGS=-vr               # Arguments to the h5ls tool
H5DELETE='h5delete -f'      # The h5delete tool name

RM='rm -rf'                 # Only for files created via stdout!
CMP='cmp -s'
DIFF='diff -c'
CP='cp'
DIRNAME='dirname'
LS='ls'
AWK='awk'

nerrors=0
verbose=yes

######################################################################
# Input files
# --------------------------------------------------------------------

# Where the tool's expected output files are located
H5MKGRP_TESTFILES_OUT_DIR="../../../tools/h5mkgrp/expected"

######################################################################
# Output files
# --------------------------------------------------------------------

# Where the text output goes
TEXT_OUTPUT_DIR=./h5mkgrp_test_output

# Where the HDF5 output from the tool goes.
HDF5_OUTPUT_DIR=./h5mkgrp_test_hdf5

######################################################################
# test files
# --------------------------------------------------------------------
# NOTE: EOL comment '#' without space can be used.
# --------------------------------------------------------------------

# Expected output files.
#
# Kept in       $H5MKGRP_TESTFILES_OUT_DIR
# Copied to     $TEXT_OUTPUT_DIR
#
EXPECTED_OUTPUT_FILES="
h5mkgrp_help.txt
h5mkgrp_single.ls
h5mkgrp_single_v.ls
h5mkgrp_single_p.ls
h5mkgrp_single_l.ls
h5mkgrp_several.ls
h5mkgrp_several_v.ls
h5mkgrp_several_p.ls
h5mkgrp_several_l.ls
h5mkgrp_nested_p.ls
h5mkgrp_nested_lp.ls
h5mkgrp_nested_mult_p.ls
h5mkgrp_nested_mult_lp.ls
"

# Generated HDF5 files.
#
# Generated in  $HDF5_OUTPUT_DIR
#
# This list is needed for cleanup since we can't simply delete a directory
# with VOL connectors that use non-file storage.
#
HDF5_OUTPUT_FILES="
h5mkgrp_single.h5
h5mkgrp_single_v.h5
h5mkgrp_single_p.h5
h5mkgrp_single_l.h5
h5mkgrp_several.h5
h5mkgrp_several_v.h5
h5mkgrp_several_p.h5
h5mkgrp_several_l.h5
h5mkgrp_nested_p.h5
h5mkgrp_nested_lp.h5
h5mkgrp_nested_mult_p.h5
h5mkgrp_nested_mult_lp.h5
"

######################################################################
# Utility functions
# --------------------------------------------------------------------

# Copy the expected text output files to the text output directory
# to make it easier to diff the expected and actual output.
#
COPY_EXPECTED_OUTPUT_FILES()
{
    for outfile in $EXPECTED_OUTPUT_FILES
    do
        filepath="$H5MKGRP_TESTFILES_OUT_DIR/$outfile"

        # ignore '#' comment
        echo $filepath | tr -d ' ' | grep '^#' > /dev/null
        RET=$?
        if [ $RET -eq 1 ]; then
            # skip cp if srcdir is same as destdir
            # this occurs when build/test performed in source dir and
            # make cp fail
            SDIR=`$DIRNAME $filepath`
            INODE_SDIR=`$LS -i -d $SDIR | $AWK -F' ' '{print $1}'`
            INODE_DDIR=`$LS -i -d $TEXT_OUTPUT_DIR | $AWK -F' ' '{print $1}'`
            if [ "$INODE_SDIR" != "$INODE_DDIR" ]; then
                # Use -f to make sure get a new copy
                $CP -f $filepath $TEXT_OUTPUT_DIR
                if [ $? -ne 0 ]; then
                    echo "Error: FAILED to copy output file: $filepath ."

                    # Comment out this to CREATE expected file
                    exit $EXIT_FAILURE
                fi
            fi
        fi
    done
}

# Cleans up HDF5 and text output from the tests. Only cleans if the
# HDF5_NOCLEANUP variable is unset.
#
CLEAN_OUTPUT()
{
    # Remove output if the "no cleanup" environment variable is not defined
    if test -z "$HDF5_NOCLEANUP"; then
        # Text output
        $RM $TEXT_OUTPUT_DIR

        # HDF5 output
        #
        # Can't just rm -rf the directory if the VOL storage doesn't map to
        # a normal file, so we'll use h5delete to delete the file.
        for hdf5file in $HDF5_OUTPUT_FILES
        do
            filepath="$HDF5_OUTPUT_DIR/$hdf5file"
            $H5DELETE $filepath
        done

        # The HDF5 output directory is always created, even if the VOL
        # storage won't use it. Delete it here.
        $RM $HDF5_OUTPUT_DIR
    fi
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING()
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Source in the output filter function definitions.
. ./output_filter.sh

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY_H5LS()
{
    SPACES="                                                               "
    echo "Verifying h5ls file structure $* $SPACES" | cut -c1-70 | tr -d '\012'
}

######################################################################
# Main testing functions
# --------------------------------------------------------------------

# Run a test and print PASS or *FAIL*. If h5mkgrp can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable.
# Assumed arguments:
#   $* arguments for h5mkgrp.
#
RUNTOOL()
{
    TESTING $H5MKGRP $@
    (
        $H5MKGRP $@
    ) > $TEXT_OUTPUT_DIR/output.out
    RET=$?
    if [ $RET != 0 ]; then
        echo "*FAILED*"
        echo "failed result is:"
        cat $TEXT_OUTPUT_DIR/output.out
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"

        # Clean up output file
        if test -z "$HDF5_NOCLEANUP"; then
            $RM $TEXT_OUTPUT_DIR/output.out
        fi
    fi
}

# Call the h5ls tool to verify the correct output data in the destination file
#
RUNH5LS()
{
    expect="$TEXT_OUTPUT_DIR/`basename $1 .h5`.ls"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .h5`.out"
    actual_sav=${actual}-sav

    # stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    VERIFY_H5LS  $@
    (
        $H5LS $H5LS_ARGS $@
    ) 2>&1 > $actual
#    ) 2>&1 | sed 's/Modified:.*/Modified:  XXXX-XX-XX XX:XX:XX XXX/' |\
#        sed 's/Location:.*/Location:  XXX:XXX/' |\
#        sed 's/with.*driver/with XXX driver/' >$actual

    # Save actual in case it is needed later.
    cp $actual $actual_sav

    # Filter output for platform-specific things
    STDOUT_FILTER $actual
    STDERR_FILTER $actual
    H5LS_FILTER $actual

    # Strip the HDF5 output directory name from the output file
    # (use | to avoid sed and directory delimiter clash)
    cp $actual $tmp_file
    sed "s|$HDF5_OUTPUT_DIR/||" < $tmp_file > $actual

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
        echo "    Expected result (*.ls) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected result (*.ls) differs from actual result (*.out)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
}

# Single run of tool
#
# Assumed arguments:
#   $1 is test file name
#   $2 is h5mkgrp options
#   $* are groups to create
#
RUNTEST()
{
    FILEOUT=$1
    shift
    H5MKGRP_ARGS=$1
    shift

    # Run test
    RUNTOOL $H5MKGRP_ARGS $FILEOUT $@

    # Verify that the file created above is correct
    RUNH5LS $FILEOUT
}

# Single run of tool
#
# Assumed arguments:
#   $1 is test expected output file
#   $2 is h5mkgrp options
#   $* are groups to create
#
CMPTEST()
{
    FILEOUT=$1
    expect="$TEXT_OUTPUT_DIR/`basename $1 .h5`.txt"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .h5`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .h5`.err"
    shift

    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    TESTING $H5MKGRP $@
    (
        $H5MKGRP $@
    ) >$actual 2>$actual_err
    cat $actual_err >> $actual

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
        echo "    Expected result (*.txt) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected result (*.txt) differs from actual result (*.out)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
}

##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
# Prepare for test
#
# We create the HDF5 output dir in case it's needed. If a VOL connector doesn't
# use normal files, it'll just stay empty and get deleted later.
CLEAN_OUTPUT
test -d $TEXT_OUTPUT_DIR || mkdir -p $TEXT_OUTPUT_DIR
test -d $HDF5_OUTPUT_DIR || mkdir -p $HDF5_OUTPUT_DIR
COPY_EXPECTED_OUTPUT_FILES

# Check that help is displayed properly
# The file name is a dummy and isn't created (an artifact from other scripts)
CMPTEST h5mkgrp_help.h5 "-h"

# Create single group at root level
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_single.h5 " " single
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_single_v.h5 "-v" single
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_single_p.h5 "-p" single
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_single_l.h5 "-l" latest

# Create several groups at root level
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_several.h5 " " one two
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_several_v.h5 "-v" one two
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_several_p.h5 "-p" one two
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_several_l.h5 "-l" one two

# Create various nested groups
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_nested_p.h5 "-p" /one/two
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_nested_lp.h5 "-lp" /one/two
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_nested_mult_p.h5 "-p" /one/two /three/four
RUNTEST $HDF5_OUTPUT_DIR/h5mkgrp_nested_mult_lp.h5 "-lp" /one/two /three/four

# Clean up generated files/directories
CLEAN_OUTPUT

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi

