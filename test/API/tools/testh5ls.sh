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
# Tests for the h5ls tool

# Assume we are on a little-endian system

srcdir=@srcdir@

WORDS_BIGENDIAN="no"

TESTNAME=h5ls
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5LS=h5ls                   # The h5ls tool name
H5DELETE='h5delete -f'      # The h5delete tool name
H5REPACK=h5repack           # The h5repack tool name

RM='rm -rf'
CMP='cmp -s'
DIFF='diff -c'
CP='cp'
NLINES=20          # Max. lines of output to display if test fails
DIRNAME='dirname'
LS='ls'
AWK='awk'

nerrors=0
verbose=yes
h5haveexitcode=yes      # default is yes

######################################################################
# Input files
# --------------------------------------------------------------------

# Where the tool's HDF5 input files are located
TESTFILES_HDF5_DIR="../../../tools/h5dump/testfiles"

# Where the tool's expected output files are located
H5LS_TESTFILES_OUT_DIR="../../../tools/h5ls/expected"

######################################################################
# Output files
# --------------------------------------------------------------------

# Where the text output goes
TEXT_OUTPUT_DIR=./h5ls_test_output

# Where the repacked HDF5 input files go
REPACK_OUTPUT_DIR=./h5ls_repack_output

######################################################################
# test files
# --------------------------------------------------------------------

# HDF5 test files.
#
# Kept in       $TESTFILES_HDF5_DIR
# Repacked to   $REPACK_OUTPUT_DIR
#
# These files fail to repack (investigate later)
# tall.h5
# tattrreg.h5 <-- This one does not even cause h5repack to fail!
# tudlink.h5
#
# These files have native-specific content
# tgrp_comments.h5
#
# Link files require an h5repack H5Lcopy work-around
# textlink.h5
# textlinksrc.h5
# textlinktar.h5
# tslink.h5
# tsoftlinks.h5
# 
# Files that give DAOS headaches for various reasons (often old-school references)
#
# tattr2.h5
# tdatareg.h5
#
# These two files basically work under DAOS, but have a minor diff error where
# h5ls prints "identical to XXX" in the native connector but not DAOS.
# thlink.h5
# tloop.h5
HDF5_FILES="
tarray1.h5
tcompound.h5
tdset.h5
tdset_idx.h5
tempty.h5
tgroup.h5
tgrpnullspace.h5
tnestedcomp.h5
tsaf.h5
tstr.h5
tvldtypes1.h5
"

# Expected output files.
#
# Kept in       $H5LS_TESTFILES_OUT_DIR
# Copied to     $TEXT_OUTPUT_DIR
#
# NOTE: This is ALL the files - they have not been culled based on the HDF5
#       files in the above list.
#
EXPECTED_OUTPUT_FILES="
help-1.ls
help-2.ls
help-3.ls
nosuchfile.err
nosuchfile.ls
tall-1.ls
tall-2.ls
tarray1.ls
tattr2.ls
tattrreg_le.ls
tattrreg_be.ls
tcomp-1.ls
tdataregbe.ls
tdataregle.ls
tdset-1.ls
tdset_idx.ls
tempty.ls
textlink-1.ls
textlinksrc-1.ls
textlinksrc-1-old.ls
textlinksrc-2.ls
textlinksrc-2-old.ls
textlinksrc-3.ls
textlinksrc-3-old.ls
textlinksrc-4.ls
textlinksrc-5.ls
textlinksrc-6.ls
textlinksrc-6-old.ls
textlinksrc-7.ls
textlinksrc-7-old.ls
textlinksrc-nodangle-1.err
textlinksrc-nodangle-1.ls
textlinksrc-nodangle-2.ls
tgroup.ls
tgroup-1.err
tgroup-1.ls
tgroup-2.ls
tgroup-3.ls
tgrp_comments.ls
tgrpnullspace.ls
thlink-1.ls
thlinks-nodangle-1.ls
tloop-1.ls
tmultifile.ls
tnestcomp-1.ls
tnestcomp-2.ls
tnestcomp-3.ls
tnestcomp-4.ls
tsaf.ls
tslink-1.ls
tsoftlinks-1.ls
tsoftlinks-2.ls
tsoftlinks-3.ls
tsoftlinks-4.ls
tsoftlinks-5.ls
tsoftlinks-nodangle-1.ls
tstr-1.ls
tudlink-1.ls
tvldtypes1.ls
tvldtypes2le.ls
tvldtypes2be.ls
"

######################################################################
# Utility functions
# --------------------------------------------------------------------

# RUNSERIAL is used. Check if it can return exit code from executable correctly.
if [ -n "$RUNSERIAL_NOEXITCODE" ]; then
    echo "***Warning*** Serial Exit Code is not passed back to shell corretly."
    echo "***Warning*** Exit code checking is skipped."
    h5haveexitcode=no
fi

# Copy the expected text output files to the text output directory
# to make it easier to diff the expected and actual output.
#
COPY_EXPECTED_OUTPUT_FILES()
{
    for outfile in $EXPECTED_OUTPUT_FILES
    do
        filepath="$H5LS_TESTFILES_OUT_DIR/$outfile"

        # Use -f to make sure get a new copy
    $CP -f $filepath $TEXT_OUTPUT_DIR
                if [ $? -ne 0 ]; then
                    echo "Error: FAILED to copy expected output file: $filepath ."
        fi
    done
}

# Repack the HDF5 files to the repack directory.
#
REPACK_HDF5_FILES()
{
    for repackfile in $HDF5_FILES
    do
        inpath="$TESTFILES_HDF5_DIR/$repackfile"
        outpath="$REPACK_OUTPUT_DIR/$repackfile"

        # Repack the file
        $H5REPACK --src-vol-name=native --enable-error-stack $inpath $outpath
        if [ $? -ne 0 ]; then
            echo "Error: FAILED to repack HDF5 file: $inpath ."
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
        for hdf5file in $HDF5_FILES
        do
            filepath="$REPACK_OUTPUT_DIR/$hdf5file"
            $H5DELETE $filepath
        done

        # The HDF5 output directory is always created, even if the VOL
        # storage won't use it. Delete it here.
        $RM $REPACK_OUTPUT_DIR
    fi
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() {
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Source in the output filter function definitions.
. ./output_filter.sh

######################################################################
# Main testing functions
# --------------------------------------------------------------------

# Run a test and print PASS or *FAIL*. For now, if h5ls can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display up to $NLINS
# lines of the actual output from the tool test.  The actual output is not
# removed if $HDF5_NOCLEANUP has a non-zero value.
# Arguments:
# $1 -- actual output filename to use
# $2 and on -- argument for the h5ls tool
RUNTEST() {
    expect="$TEXT_OUTPUT_DIR/$1"
    expect_err="$TEXT_OUTPUT_DIR/`basename $1 .ls`.err"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .ls`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .ls`.out.err"
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
    shift
    retvalexpect=$1
    shift

    # Run test.
    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    TESTING $H5LS $@
    (
        $RUNSERIAL $H5LS "$@"
    ) >$actual 2>$actual_err
    exitcode=$?

    # Clean h5ls stdout files
    H5LS_FILTER $actual

    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err
    if [ $h5haveexitcode = 'yes' -a $exitcode -ne $retvalexpect ]; then
        echo "*FAILED*"
        nerrors="`expr $nerrors + 1`"
        if [ yes = "$verbose" ]; then
            echo "test returned with exit code $exitcode"
            echo "test output: (up to $NLINES lines)"
            head -$NLINES $actual
            echo "***end of test output***"
            echo ""
        fi
    elif [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
        echo "    Expected result (*.ls) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected result differs from actual result"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
}

##############################################################################
##############################################################################
###              T H E   T E S T S                                         ###
##############################################################################
##############################################################################

# Prepare for test
#
# We create the repack output dir in case it's needed. If a VOL connector doesn't
# use normal files, it'll just stay empty and get deleted later.
CLEAN_OUTPUT
test -d $TEXT_OUTPUT_DIR || mkdir -p $TEXT_OUTPUT_DIR
test -d $REPACK_OUTPUT_DIR || mkdir -p $REPACK_OUTPUT_DIR
COPY_EXPECTED_OUTPUT_FILES
REPACK_HDF5_FILES

# Run h5ls tests

# test the help syntax
RUNTEST help-1.ls 0 -w80 -h
RUNTEST help-2.ls 0 -w80 -help
RUNTEST help-3.ls 0 -w80 -?

# test simple command (TODO: the tall file does not repack)
#RUNTEST tall-1.ls 0 -w80 $REPACK_OUTPUT_DIR/tall.h5
#RUNTEST tall-2.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tall.h5
RUNTEST tgroup.ls 0 -w80 $REPACK_OUTPUT_DIR/tgroup.h5
RUNTEST tgroup-3.ls 0 -w80 $REPACK_OUTPUT_DIR/tgroup.h5/g1

# test for displaying groups
# The following combination of arguments is expected to return an error message
# and return value 1
#RUNTEST tgroup-1.ls 1 -w80 -r -g $REPACK_OUTPUT_DIR/tgroup.h5
RUNTEST tgroup-2.ls 0 -w80 -g $REPACK_OUTPUT_DIR/tgroup.h5/g1

# test for files with groups that have long comments (native only)
#RUNTEST tgrp_comments.ls 0 -w80 -v -g $REPACK_OUTPUT_DIR/tgrp_comments.h5/glongcomment

# test for displaying simple space datasets
RUNTEST tdset-1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tdset.h5

# test for displaying soft links
#RUNTEST tslink-1.ls 0 -w80 -r $REPACK_OUTPUT_DIR/tslink.h5

# test for displaying more soft links with --follow-symlinks
#RUNTEST tsoftlinks-1.ls 0 --follow-symlinks $REPACK_OUTPUT_DIR/tsoftlinks.h5
#RUNTEST tsoftlinks-2.ls 0 --follow-symlinks -r $REPACK_OUTPUT_DIR/tsoftlinks.h5
#RUNTEST tsoftlinks-3.ls 0 --follow-symlinks $REPACK_OUTPUT_DIR/tsoftlinks.h5/group1
#RUNTEST tsoftlinks-4.ls 0 --follow-symlinks -r $REPACK_OUTPUT_DIR/tsoftlinks.h5/group1
#RUNTEST tsoftlinks-5.ls 0 --follow-symlinks $REPACK_OUTPUT_DIR/tsoftlinks.h5/soft_dset1

# test for displaying external and user-defined links with --follow-symlinks
# TODO: textlinksrc-2.ls has a diff error from a path that needs to be cleaned
# TODO: tudlink fails to repack
#RUNTEST textlink-1.ls 0 -w80 -r $REPACK_OUTPUT_DIR/textlink.h5
#RUNTEST textlinksrc-1.ls 0 -w80 --follow-symlinks -r $REPACK_OUTPUT_DIR/textlinksrc.h5
#RUNTEST textlinksrc-2.ls 0 -w80 --follow-symlinks -rv $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link5
#RUNTEST textlinksrc-3.ls 0 -w80 --follow-symlinks -r $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link1
#RUNTEST textlinksrc-4.ls 0 -w80 -r $REPACK_OUTPUT_DIR/textlinksrc.h5
#RUNTEST textlinksrc-5.ls 0 -w80 -r $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link1
#RUNTEST textlinksrc-6.ls 0 -w80 --follow-symlinks $REPACK_OUTPUT_DIR/textlinksrc.h5
#RUNTEST textlinksrc-7.ls 0 -w80 --follow-symlinks $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link1
#RUNTEST tudlink-1.ls 0 -w80 -r tudlink.h5

# test for displaying external links with -E
# the option -E will be depreciated but keep it for backward compatibility
# TODO: textlinksrc-2.ls has a diff error from a path that needs to be cleaned
#RUNTEST textlinksrc-1-old.ls 0 -w80 -Er $REPACK_OUTPUT_DIR/textlinksrc.h5
#RUNTEST textlinksrc-2-old.ls 0 -w80 -Erv $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link5
#RUNTEST textlinksrc-3-old.ls 0 -w80 -Er $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link1
#RUNTEST textlinksrc-6-old.ls 0 -w80 -E $REPACK_OUTPUT_DIR/textlinksrc.h5
#RUNTEST textlinksrc-7-old.ls 0 -w80 -E $REPACK_OUTPUT_DIR/textlinksrc.h5/ext_link1

# tests for no-dangling-links
# if this option is given on dangling link, h5ls should return exit code 1
# when used alone , expect to print out help and return exit code 1
#RUNTEST textlinksrc-nodangle-1.ls 1 -w80 --no-dangling-links $REPACK_OUTPUT_DIR/textlinksrc.h5
# external dangling link - expected exit code 1
#RUNTEST textlinksrc-nodangle-2.ls 1 -w80 --follow-symlinks --no-dangling-links $REPACK_OUTPUT_DIR/textlinksrc.h5
# soft dangling link - expected exit code 1
#RUNTEST tsoftlinks-nodangle-1.ls 1 -w80 --follow-symlinks --no-dangling-links $REPACK_OUTPUT_DIR/tsoftlinks.h5
# when used file with no dangling links - expected exit code 0
#RUNTEST thlinks-nodangle-1.ls 0 -w80 --follow-symlinks --no-dangling-links $REPACK_OUTPUT_DIR/thlink.h5

# test for wildcards in filename (does not work with cmake)
# this h5ls test script does not pass the filename properly like the h5dump test script???
# The two tests below are commented out in the develop HDF5 branch
#RUNTEST tstarfile.ls 0 -w80 t*link.h5
#RUNTEST tqmarkfile.ls 0 -w80 t?link.h5
#RUNTEST tmultifile.ls 0 -w80 $REPACK_OUTPUT_DIR/thlink.h5 $REPACK_OUTPUT_DIR/tslink.h5

# tests for hard links
#RUNTEST thlink-1.ls 0 -w80 $REPACK_OUTPUT_DIR/thlink.h5

# tests for compound data types
RUNTEST tcomp-1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tcompound.h5

#test for the nested compound type
RUNTEST tnestcomp-1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tnestedcomp.h5

RUNTEST tnestcomp-2.ls 0 -w80 -r -d -S $REPACK_OUTPUT_DIR/tnestedcomp.h5

RUNTEST tnestcomp-3.ls 0 -w80 -r -d -l $REPACK_OUTPUT_DIR/tnestedcomp.h5

RUNTEST tnestcomp-4.ls 0 -w80 -r -d -l -S $REPACK_OUTPUT_DIR/tnestedcomp.h5

# test for loop detection
#RUNTEST tloop-1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tloop.h5

# test for string
RUNTEST tstr-1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tstr.h5

# test test file created from lib SAF team
RUNTEST tsaf.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tsaf.h5

# test for variable length data types
RUNTEST tvldtypes1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tvldtypes1.h5

# test for array data types
RUNTEST tarray1.ls 0 -w80 -r -d $REPACK_OUTPUT_DIR/tarray1.h5

# test for empty data
RUNTEST tempty.ls 0 -w80 -d $REPACK_OUTPUT_DIR/tempty.h5

# test for displaying dataset and attribute of null space
# TODO: This has a path that needs cleaning
#RUNTEST tgrpnullspace.ls 0 -w80 -v -S $REPACK_OUTPUT_DIR/tgrpnullspace.h5

# test for all dataset types written to attributes
# enable -S for avoiding printing NATIVE types
# TODO: This has a path that needs cleaning
#RUNTEST tattr2.ls 0 -w80 -v -S $REPACK_OUTPUT_DIR/tattr2.h5

# test for attribute with region references without verbose mode
# ( HDFFV-7838, )
# TODO: The tattrreg file fails to repack
#if test $WORDS_BIGENDIAN != "yes"; then
#    RUNTEST tattrreg_le.ls 0 -w80 -v -d $REPACK_OUTPUT_DIR/tattrreg.h5
#else
#    RUNTEST tattrreg_be.ls 0 -w80 -v -d $REPACK_OUTPUT_DIR/tattrreg.h5
#fi

# tests for error handling.
# test for non-existing file
#RUNTEST nosuchfile.ls 1 $REPACK_OUTPUT_DIR/nosuchfile.h5

# test for variable length data types in verbose mode
# TODO: Diff fails on hard-coded dataset locations.
#if test $WORDS_BIGENDIAN != "yes"; then
 #    RUNTEST tvldtypes2le.ls 0 -v $REPACK_OUTPUT_DIR/tvldtypes1.h5
#else
 #    RUNTEST tvldtypes2be.ls 0 -v $REPACK_OUTPUT_DIR/tvldtypes1.h5
#fi

# test for dataset region references data types in verbose mode
# TODO: Diff fails on hard-coded dataset locations.
#if test $WORDS_BIGENDIAN != "yes"; then
 #    RUNTEST tdataregle.ls 0 -v $REPACK_OUTPUT_DIR/tdatareg.h5
#else
 #    RUNTEST tdataregbe.ls 0 -v $REPACK_OUTPUT_DIR/tdatareg.h5
#fi

# Clean up generated files/directories
CLEAN_OUTPUT

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi

