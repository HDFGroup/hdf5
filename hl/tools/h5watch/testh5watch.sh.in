#! /bin/sh
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
# Tests for the h5watch tool
#

# Check to see if the VFD specified by the HDF5_DRIVER environment variable
# supports SWMR.
./swmr_check_compat_vfd
rc=$?
if [[ $rc != 0 ]] ; then
    echo
    echo "The VFD specified by the HDF5_DRIVER environment variable"
    echo "does not support SWMR."
    echo
    echo "h5watch tests skipped"
    echo
    exit 0
fi

#echo "h5watch tests are skipped temporarily."
#echo
#exit 0

H5WATCH=h5watch                   # The tool name
H5WATCH_BIN=`pwd`/$H5WATCH        # The path of H5WATCH
EXTEND_DSET=extend_dset     # Routine to extend the dataset when watching
EXTEND_BIN=`pwd`/$EXTEND_DSET     # The path of EXTEND_DSET
#
EXIT_SUCCESS=0
EXIT_FAILURE=1
#
GEN_TEST=h5watchgentest              # Generate HDF5 file with various datasets
GEN_TEST_BIN=`pwd`/$GEN_TEST     # Path of the binary GEN_TEST
WATCHFILE=`pwd`/WATCH.h5        # The HDF5 file generated to test h5watch
TESTFILE=TEST.h5                # The temporary file (a copy of WATCH.h5) used by tests
TRY_MAX=30                      # Try running the test again
#
# These 3 defines should be the same as the defines in ./extend_dset.c
WRITER_MESSAGE=writer_message   # The message file created by the "extend" process
READER_MESSAGE=reader_message   # The message file created by the "watch" process
MESSAGE_TIMEOUT=300             # Message timeout length in secs
#
CMP='cmp -s'
DIFF='diff -c'
NLINES=20            # Max. lines of output to display if test fails
#
# Mac OS: just to make sure echo "backslash backslash" behaves properly
if test `uname -s` = 'Darwin'; then
    ECHO='/bin/echo'
else
    ECHO='echo'
fi
#
# Global variables
nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi
test -d ../testfiles || mkdir ../testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() {
    SPACES="                                                               "
    $ECHO "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

#
# Overall algorithm:
#
# Run a test and print PASSED or FAILED
# If a test did not return with the expected return code,
# increment the `nerrors' global variable and (if $verbose is set) display up to $NLINES
# lines of the actual output from the test.
# If the test did return the expected code,
# compare the actual output with the expected output;
# If the outputs are the same, print PASSED,
# Otherwise print FAILED and the difference between the two outputs.
# The output files are not removed if $HDF5_NOCLEANUP has a non-zero value.
#
#
# TOOLTEST():
#
# Arguments:
#
# $1 -- expected output
# $2 -- expected return code
# $3 and on -- arguments for h5watch
TOOLTEST() {
    expect="$srcdir/../testfiles/$1"
    actual="../testfiles/`basename $1 .ddl`.out"
    actual_err="../testfiles/`basename $1 .ddl`.err"
    shift
    retvalexpect=$1
    shift
    # Run test.
    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    TESTING $H5WATCH $@
    (
        $RUNSERIAL $H5WATCH_BIN "$@"
    ) > $actual 2>$actual_err
    exitcode=$?
    cat $actual_err >> $actual
    if [ $exitcode -ne $retvalexpect ]; then
        $ECHO "*FAILED*"
        nerrors="`expr $nerrors + 1`"
        if [ yes = "$verbose" ]; then
            $ECHO "test returned with exit code $exitcode"
            $ECHO "test output: (up to $NLINES lines)"
            head -$NLINES $actual
            $ECHO "***end of test output***"
            $ECHO ""
        fi
        elif $CMP $expect $actual; then
            $ECHO " PASSED"
        else
            $ECHO "*FAILED*"
            $ECHO "    Expected result differs from actual result"
            nerrors="`expr $nerrors + 1`"
            test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
        fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
    rm -f $actual $actual_err
    fi
}
#
# TOOLTEST_ERR():
# same as toolset only compare error file
#
# Arguments:
#
# $1 -- expected output
# $2 -- expected return code
# $3 and on -- arguments for h5watch
TOOLTEST_ERR() {
    expect="$srcdir/../testfiles/$1"
    expect_err="$srcdir/../testfiles/`basename $1 .ddl`.err"
    actual="../testfiles/`basename $1 .ddl`.out"
    actual_err="../testfiles/`basename $1 .ddl`.out.err"
    shift
    retvalexpect=$1
    shift
    # Run test.
    TESTING $H5WATCH $@
    (
        $RUNSERIAL $H5WATCH_BIN "$@"
    ) > $actual 2>$actual_err
    exitcode=$?
    if [ $exitcode -ne $retvalexpect ]; then
        $ECHO "*FAILED*"
        nerrors="`expr $nerrors + 1`"
        if [ yes = "$verbose" ]; then
            $ECHO "test returned with exit code $exitcode"
            $ECHO "test output: (up to $NLINES lines)"
            head -$NLINES $actual
            $ECHO "***end of test output***"
            $ECHO ""
        fi
        elif $CMP $expect_err $actual_err; then
            $ECHO " PASSED"
        else
            $ECHO "*FAILED*"
            $ECHO "    Expected result differs from actual result"
            nerrors="`expr $nerrors + 1`"
            test yes = "$verbose" && $DIFF $expect_err $actual_err |sed 's/^/    /'
        fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
    rm -f $actual $actual_err
    fi
}

#
#
#
# TEST_WATCH():
#
# Arguments:
#       $1 -- the specified dataset to watch and to extend
#       $2 -- the options to h5watch (can be NULL)
#       $3 -- expected output from watching the object
#       $4 -- action 1
#       $5 -- action 2
#
TEST_WATCH() {
try=0
while [ $try -lt $TRY_MAX ] ;
do
    cp $WATCHFILE $TESTFILE                             # Copy the file being watched/extended to a temporary file
    actual="../testfiles/`basename $3 .ddl`.out"        # The actual output
    expect="$srcdir/../testfiles/$3"                    # The expected output
    #
    # Set up options and object to h5watch
    if test -z "$2"; then
        OBJ="$TESTFILE/$1"                              # Empty options, just object to h5watch
    else
        OBJ="$2 $TESTFILE/$1"                           # Options + object to h5watch
    fi
    rm -f $WRITER_MESSAGE                               # Remove the file just to be sure
    rm -f $READER_MESSAGE                               # Remove the file just to be sure
    #
    $EXTEND_BIN $TESTFILE $1 $4 $5&                     # Extend the dataset; put in background
    extend_pid=$!                                       # Get "extend" process ID
    #
    # Wait for message from "extend_dset" process to start h5watch--
    # To wait for the writer message file or till the maximum # of seconds is reached
    # This performs similar function as the routine h5_wait_message() in test/h5test.c
    mexist=0                                        # Indicate whether the message file is found
    t0=`date +%s`                                   # Get current time in seconds
    difft=0                                         # Initialize the time difference
    while [ $difft -lt $MESSAGE_TIMEOUT ] ;         # Loop till message times out
    do
        t1=`date +%s`                               # Get current time in seconds
        difft=`expr $t1 - $t0`                      # Calculate the time difference
        if [ -e $WRITER_MESSAGE ]; then             # If message file is found:
            mexist=1                                #       indicate the message file is found
            rm $WRITER_MESSAGE                      #       remove the message file
            break                                   #       get out of the while loop
        fi
    done;
    #
    # If message file from "extend" process is found--
    #   start h5watch
    #   send message to "extend" process to start work
    #   wait for "extend" process to complete, then kill h5watch
    #   determine test result
    # If message file from "extend" process is not found--
    #   there is some problem; the test fails
    #
    if test $mexist -eq 0; then
        $ECHO "*FAILED*"
        $ECHO "Problem with extend_dset...this test failed."
    else
        #
        # Run h5watch; put in background; collect output to a file
        TESTING $H5WATCH $OBJ
        $RUNSERIAL $H5WATCH_BIN $2 "$TESTFILE/$1" >> $actual &
        watch_pid=$!                                    # Get h5watch process ID
        cp /dev/null $READER_MESSAGE                    # Send message to "extend" process to start work
        wait $extend_pid                                # Wait for "extend" process to complete
        extend_exit=$?                                  # Collect "extend" process' exit code
        sleep 1                                         # Sleep to make sure output is flushed
        kill $watch_pid                                  # Kill h5watch
        wait $watch_pid                                    # Wait for "h5watch" process to complete
        #
        if [ $extend_exit -ne 0 ]; then                 # Error returned from "extend" process
            $ECHO "*FAILED*"
            nerrors="`expr $nerrors + 1`"
            if [ yes = "$verbose" ]; then
                $ECHO "extend test returned with exit code $extend_exit"
                $ECHO "test output: (up to $NLINES lines)"
                head -$NLINES $actual
                $ECHO "***end of test output***"
                $ECHO ""
            fi
        elif $CMP $expect $actual; then                 # Compare actual output with expected output
            try=$TRY_MAX
            $ECHO " PASSED"
        else
            try="`expr $try + 1`"
            if [ $try -lt $TRY_MAX ]; then
                $ECHO "*RETRY"
                rm -f $actual
                rm -f $TESTFILE
            else
                $ECHO "*FAILED*"                            # Actual and expected outputs are different
                $ECHO "    Expected result differs from actual result"
                nerrors="`expr $nerrors + 1`"
                if test yes = "$verbose"; then
                    $DIFF $expect $actual |sed 's/^/    /'
                fi
            fi
        fi
        #
        # Cleaning
        rm -f $TESTFILE
        if test -z "$HDF5_NOCLEANUP"; then
            rm -f $actual
        fi
    fi
done;
}
##############################################################################
##############################################################################
###                     T H E   T E S T S                                  ###
##############################################################################
##############################################################################
#
#
#################################################################################################
#                                                                                               #
# WATCH.h5: file with various types of datasets for testing--                                   #
#   The following datasets are chunked, H5D_ALLOC_TIME_INCR, max. dimensional setting:          #
#       DSET_ONE: one-dimensional dataset                                                       #
#       DSET_TWO: two-dimensional dataset                                                       #
#       DSET_CMPD: one-dimensional dataset with compound type                                   #
#       DSET_CMPD_ESC: one-dimensional dataset with compound type & escape/separator characters    #
#       DSET_CMPD_TWO: two-dimensional dataset with compound type                               #
#                                                                                               #
#   The following datasets are one-dimensional, chunked, max. dimension setting:                #
#       DSET_ALLOC_EARLY: dataset with H5D_ALLOC_TIME_EARLY                                     #
#       DSET_ALLOC_LATE: dataset H5D_ALLOC_TIME_LATE                                            #
#                                                                                               #
#   The following datasets are one-dimensional:                                                 #
#       DSET_NONE: fixed dimension setting, contiguous, H5D_ALLOC_TIME_LATE                     #
#       DSET_NOMAX: fixed dimension setting, chunked, H5D_ALLOC_TIME_INCR                       #
#                                                                                               #
#################################################################################################
#
#
#################################################################################################
#                                                                                               #
# Tests on expected failures:                                                                   #
#    Invalid file name                                                                           #
#    Unable to find dataset, invalid dataset                                                     #
#      DSET_NONE and DSET_NOMAX                                                                  #
#    Invalid input to options --width and --polling                                              #
#    Invalid field names for -f option                                                           #
#                                                                                               #
#################################################################################################
#
# Generate file with various types of datasets
$GEN_TEST_BIN
# Test on --help options
TOOLTEST w-help1.ddl 0 --help
#
# Tests on expected failures
TOOLTEST_ERR w-err-dset1.ddl 1 WATCH.h5
TOOLTEST_ERR w-err-dset2.ddl 1 WATCH.h5/group/DSET_CMPD
TOOLTEST_ERR w-err-dset-none.ddl 1 WATCH.h5/DSET_NONE
TOOLTEST_ERR w-err-dset-nomax.ddl 1 WATCH.h5/DSET_NOMAX
TOOLTEST_ERR w-err-file.ddl 1 ../WATCH.h5/DSET_CMPD
TOOLTEST w-err-width.ddl 1 --width=-8 WATCH.h5/DSET_ONE
TOOLTEST w-err-poll.ddl 1 --polling=-8 WATCH.h5/DSET_ONE
TOOLTEST w-err-poll0.ddl 1 --polling=0 WATCH.h5/DSET_ONE
#
# Tests on invalid field names via --fields option for a compound typed dataset: DSET_CMPD
TOOLTEST_ERR w-err-cmpd1.ddl 1 --fields=fieldx WATCH.h5/DSET_CMPD
TOOLTEST_ERR w-err-cmpd2.ddl 1 --fields=field1,field2. WATCH.h5/DSET_CMPD
TOOLTEST_ERR w-err-cmpd3.ddl 1 --fields=field1,field2, WATCH.h5/DSET_CMPD
TOOLTEST_ERR w-err-cmpd4.ddl 1 --fields=field1,field2.b.k WATCH.h5/DSET_CMPD
TOOLTEST_ERR w-err-cmpd5.ddl 1 --fields=field1 --fields=field2.b.k WATCH.h5/DSET_CMPD
#
echo "DONE WITH 1st SET OF TESTS"
#
#
#
#################################
# Tests without options         #
#################################
#
# Generate file WATCH.h5 with various types of datasets,
$GEN_TEST_BIN
#
# Watching and extending: (TEST.h5 is a copy of WATCH.h5)
#       TEST.h5/DSET_ONE
#       TEST.h5/DSET_ALLOC_EARLY
#       TEST.h5/DSET_ALLOC_LATE
#       TEST.h5/DSET_CMPD
#       TEST.h5/DSET_TWO
#       TEST.h5/DSET_CMPD_TWO
#       TEST.h5/DSET_CMPD_ESC
#
TEST_WATCH DSET_ONE '' w-ext-one.ddl 3 0            #Increase
TEST_WATCH DSET_ALLOC_EARLY '' w-ext-early.ddl -1 0 #Decrease
TEST_WATCH DSET_ALLOC_LATE '' w-ext-late.ddl 0 0    #Same
TEST_WATCH DSET_CMPD '' w-ext-cmpd.ddl 3 0          #Increase
TEST_WATCH DSET_CMPD_ESC '' w-ext-cmpd-esc.ddl -1 0 #Decrease
TEST_WATCH DSET_TWO '' w-ext-two.ddl 2 2            #Increase, Increase
TEST_WATCH DSET_CMPD_TWO '' w-ext-cmpd-two.ddl 2 -9 #Increase, Decrease
#
echo "DONE WITH 2nd SET OF TESTS"
#
#
#
#################################
# Tests on --fields option      #
#################################
#
# Watching and extending: (TEST.h5 is a copy of WATCH.h5)
#       TEST.h5/DSET_CMPD with --fields=field1,field2
#       TEST.h5/DSET_CMPD with --fields=field2.b,field4
#       TEST.h5/DSET_CMPD with --fields=field2.b.a --fields=field2.c
TEST_WATCH DSET_CMPD --fields=field1,field2 w-ext-cmpd-f1.ddl -9 0                  #Decrease
TEST_WATCH DSET_CMPD --fields=field2.b,field4 w-ext-cmpd-f2.ddl 3 0                 #Increase
TEST_WATCH DSET_CMPD '--fields=field2.b.a --fields=field2.c' w-ext-cmpd-ff3.ddl 0 0 #Same
#
#
#       TEST.h5/DSET_CMP_TWO with --fields=field1,field2
#       TEST.h5/DSET_CMPD_TWO with --fields=field2.b --fields=field4
#       TEST.h5/DSET_CMPD_TWO with --fields=field2.b.a,field2.c
TEST_WATCH DSET_CMPD_TWO --fields=field1,field2 w-ext-cmpd-two-f1.ddl   2 0                 #Increase, Same
TEST_WATCH DSET_CMPD_TWO '--fields=field2.b --fields=field4' w-ext-cmpd-two-ff2.ddl -1 2    #Decrease, Increase
TEST_WATCH DSET_CMPD_TWO --fields=field2.b.a,field2.c w-ext-cmpd-two-f3.ddl -1 -3           #Decrease, Decrease
#
#
#       TEST.h5/DSET_CMPD_ESC with --fields=field\,1,field2\.
#       TEST.h5/DSET_CMPD_ESC with --fields=field2\..\,b --fields=field4\,
#       TEST.h5/DSET_CMPD_ESC with --fields=field2\..\,b.a,field2\..\\K
TEST_WATCH DSET_CMPD_ESC '--fields=field\,1,field2\.' w-ext-cmpd-esc-f1.ddl 3 0                 #Increase
TEST_WATCH DSET_CMPD_ESC '--fields=field2\..\,b --fields=field4\,' w-ext-cmpd-esc-ff2.ddl -1 0  #Decrease
TEST_WATCH DSET_CMPD_ESC '--fields=field2\..\,b.a,field2\..\\K' w-ext-cmpd-esc-f3.ddl 3 0       #Increase
#
#
echo "DONE WITH 3rd SET OF TESTS"
#
#
#
#################################################
# Tests on options:                             #
#       --dim                                   #
#       --width, --label, --simple, --help      #
#################################################
#
# Watching and extending: (TEST.h5 is a copy of WATCH.h5)
#       TEST.h5/DSET_ONE with -d option
#       TEST.h5/DSET_TWO with --dim option
#       TEST.h5/DSET_TWO with --width=60 option
#       TEST.h5/DSET_CMPD with --label option
#       TEST.h5/DSET_ONE with --simple option
TEST_WATCH DSET_ONE --dim w-ext-one-d.ddl 3 0           #Increase
TEST_WATCH DSET_TWO --dim w-ext-two-d.ddl -2 0          #Decrease, Same
TEST_WATCH DSET_TWO --width=30 w-ext-two-width.ddl 0 2  #Same, Increase
TEST_WATCH DSET_CMPD --label w-ext-cmpd-label.ddl 3 0   #Increase
TEST_WATCH DSET_ONE --simple w-ext-one-simple.ddl 2 0   #I
#
echo "DONE WITH 4th SET OF TESTS"
#
#
#
if test $nerrors -eq 0 ; then
    $ECHO "All h5watch tests passed."
    exit $EXIT_SUCCESS
else
    $ECHO "h5watch tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
