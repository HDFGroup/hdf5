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
#
# Test script for the flush/evict single objects feature.
#
# This test file doesn't actually perform any tests, rather, it acts
# as a process manager for the 'flushrefresh' test file, which is where
# the tests are actually housed. The reason this script exists is because
# the verification of this feature needs to occur in separate processes
# from the one in which the file is being manipulated in. (i.e., we have
# a single writer process, and various reader processes spawning off
# and doing the verification that individual objects are being
# correctly flushed).
#
# Programmer:
#   Mike McGreevy
#   Tuesday, July 20, 2010

###############################################################################
## test variables
###############################################################################

# Number of errors encountered during test run.
nerrors=0

# Set up a function to check the current time since the epoch - ideally, we'd
# like to use Perl. If it wasn't detected by configure, then use date, though
# this is less portable and might cause problems on machines that don't
# recognize the +%s option (like Solaris).
#
# Note that PERL will resolve to true or false, not a path.
PERL=@PERL@
if test -n "$PERL"; then
    TimeStamp()
    {
        time=`perl -e 'print int(time)'`
        echo "$time"
    }
else
    TimeStamp()
    {
        time=`date +%s`
        echo "$time"
    }
fi

###############################################################################
## Main
###############################################################################
# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

# Check to see if the VFD specified by the HDF5_DRIVER environment variable
# supports SWMR.
./swmr_check_compat_vfd
rc=$?
if [ $rc -ne 0 ] ; then
    echo
    echo "The VFD specified by the HDF5_DRIVER environment variable"
    echo "does not support SWMR."
    echo
    echo "flush/refresh tests skipped"
    echo
    exit 0
fi

# HDF5 has several tests that create and delete signal files to communicate
# between processes, and it seems that even though the names of the files are
# different, occasionally the wrong file is deleted, interrupting the flow of
# the test.  Running each of these tests in its own directory should eliminate
# the problem.
mkdir flushrefresh_test
cp flushrefresh flushrefresh_test

# With the --disable-shared option, flushrefresh is built in the test directory,
# otherwise it is in test/.libs with a wrapper script named flushrefresh in
# the test directory.  test/flushrefresh should always be copied,
# .libs/flushrefresh should be copied only if it exists.
if [ -f .libs/flushrefresh ]; then
    mkdir flushrefresh_test/.libs
    for FILE in .libs/flushrefresh*; do
        case "$FILE" in
            *.o) continue ;;    ## don't copy the .o files
        esac
        if test -f "$FILE" ; then
            cp $FILE flushrefresh_test/.libs
        fi
    done
fi
cd flushrefresh_test

# =================================================
# Set up/initialize some variables to be used later
# =================================================
testfile=flushrefresh.h5
startsignal=flushrefresh_VERIFICATION_START
endsignal=flushrefresh_VERIFICATION_DONE
timeout_length=300
timedout=0
verification_done=0
if [ -e $testfile ]; then
    rm $testfile
fi

# ========================
# Launch the Test Program.
# ========================
./flushrefresh &
pid_main=$!

# =======================================
# Run flush verification on test program.
# =======================================

until [ $verification_done -eq 1 ]; do

  # Wait for signal from test program that verification routine can run.
  before=`TimeStamp`
  until [ -s $startsignal ]; do
    after=`TimeStamp`
    timediff=`expr $after - $before`
    if [ $timediff -gt $timeout_length ]; then
        nerrors=`expr $nerrors + 1`
        timedout=1
        break
    fi
  done

  # Check to see if we timed out looking for the signal before continuing.
  if [ $timedout -gt 0 ]; then
    echo "timed out waiting for signal from test program (flush)."
    break
  fi

  # Read in test routine parameters from signal file, then delete signal file.
  param1=`head -1 $startsignal`
  param2=`tail -1 $startsignal`
  rm $startsignal

  # Check if we're done with verifications, otherwise run the specified verification.
  if [ "$param1" = "VERIFICATION_DONE" ]; then
    verification_done=1
    echo "all flush verification complete" > $endsignal
  else
    ./flushrefresh $param1 $param2
    echo "verification flush process done" > $endsignal
  fi

done

# =========================================
# Run refresh verification on test program.
# =========================================
if [ $timedout -eq 0 ]; then
  until [ $verification_done -eq 2 ]; do

    # Wait for signal from test program that verification routine can run.
    before=`TimeStamp`
    until [ -s $startsignal ]; do
      after=`TimeStamp`
      timediff=`expr $after - $before`
      if [ $timediff -gt $timeout_length ]; then
          nerrors=`expr $nerrors + 1`
          timedout=1
          break
      fi
    done

    # Check to see if we timed out looking for the signal before continuing.
    if [ $timedout -gt 0 ]; then
      echo "timed out waiting for signal from test program (refresh)."
      break
    fi

    # Read in test routine parameter from signal file, then delete signal file.
    param1=`head -n 1 $startsignal`
    rm $startsignal

    # Check if we're done with verifications, otherwise run the specified verification.
    if [ "$param1" = "VERIFICATION_DONE" ]; then
      verification_done=2
      echo "all refresh verification complete" > $endsignal
    else
      ./flushrefresh $param1
      echo "refresh verifiction process done" > $endsignal
    fi

  done
fi

# ============================================
# Wait for main to finish up, and end testing.
# ============================================
wait $pid_main
if test $? -ne 0; then
    echo flushrefresh had error
    nerrors=`expr $nerrors + 1`
fi

###############################################################################
## Report and exit
###############################################################################

if test $nerrors -eq 0 ; then
    echo "flush/refresh objects tests passed."
    if test -z "$HDF5_NOCLEANUP"; then
        # delete the test directory
        rm -rf flushrefresh_test
    fi
    exit 0
else
    echo "flush/refresh objects tests failed with $nerrors errors."
    exit 1
fi
