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
# Tests for journaling related marking and unmarking of HDF5 files.
#
# These tests used to be in cache2_journal.c, but had to be moved 
# out as the tests require simulated crashes, and it is difficult to
# do this in a C program without using fork().

nerrors=0

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ./testfiles || mkdir ./testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
    SPACES="                                                               "
    echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable.
#
TEST() {
   TEST_ERR=$1                  # The test name
   TEST_ERR_BIN=`pwd`/$TEST_ERR # The path of the test binary
   TEST_DESC=$2

   #Run the test:
   trap "" 6
   $RUNSERIAL $TEST_ERR_BIN $TEST_DESC setup
   trap 6
   TESTING $TEST_DESC
   $RUNSERIAL $TEST_ERR_BIN $TEST_DESC check
   if [ $? -eq 0 ] 
   then
      echo " PASSED"
   else
      echo "*FAILED*"
      nerrors="`expr $nerrors + 1`"
   fi
}

# Print a "SKIP" message
SKIP() {
    TESTING $@
    echo  " -SKIP-"
}
  
##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

echo "Tests to verify correct marking of journaling in progress status in a"
echo "HDF5 file under various conditions -- most involving abnormal exits."
echo "Thus the \"Aborted\" messages between tests are expected."

TEST cache2_jnl_file_marking file_marking_after_open
TEST cache2_jnl_file_marking file_marking_on_create
TEST cache2_jnl_file_marking file_marking_on_open
TEST cache2_jnl_file_marking file_unmarking_on_file_close
TEST cache2_jnl_file_marking file_unmarking_on_journaling_shutdown
TEST cache2_jnl_file_marking file_unmarking_on_recovery

if test $nerrors -eq 0 ; then
   echo "All journaling file marking tests passed."
fi

exit $nerrors
