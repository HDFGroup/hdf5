#! /bin/sh
# 
#  Copyright (C) 1998-2002 National Center for Supercomputing Applications
#                          All rights reserved.
#
# Tests for the h5diff tool

DUMPER=h5diff               # The tool name
DUMPER_BIN=`pwd`/$DUMPER    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi


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
   # Run test.
   TESTING $RUNSERIAL $DUMPER_BIN "$@"
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test 
TOOLTEST -d dset h5diff_test1.h5 h5diff_test2.h5

exit $nerrors
