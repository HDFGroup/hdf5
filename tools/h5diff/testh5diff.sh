#! /bin/sh
#
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
#
# Tests for the h5diff tool

H5DIFF=h5diff               # The tool name
H5DIFF_BIN=`pwd`/$H5DIFF    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ../testfiles || mkdir ../testfiles

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
   expect="$srcdir/../testfiles/$1"
   actual="../testfiles/`basename $1 .txt`.out"
   actual_err="../testfiles/`basename $1 .txt`.err"
   shift

   # Run test.
   # Tflops interprets "$@" as "" when no parameter is given (e.g., the
   # case of missing file name).  Changed it to use $@ till Tflops fixes it.
   TESTING $H5DIFF $@
   (
      echo "#############################"
      echo "Expected output for '$H5DIFF $@'" 
      echo "#############################"
      cd $srcdir/../testfiles
      if [ "`uname -s`" = "TFLOPS O/S" ]; then
        $RUNSERIAL $H5DIFF_BIN $@
      else
        $RUNSERIAL $H5DIFF_BIN "$@"
      fi
   ) >$actual 2>$actual_err
   cat $actual_err >> $actual
    
   if $CMP $expect $actual; then
      echo " PASSED"
   else
      echo "*FAILED*"
      echo "    Expected result (*.txt) differs from actual result (*.out)"
      nerrors="`expr $nerrors + 1`"
      test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
   fi

   # Clean up output file
#   if test -z "$HDF5_NOCLEANUP"; then
#      rm -f $actual $actual_err
#   fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test1: Check if the command line number of arguments is less than 3
TOOLTEST h5diff_1.txt h5diff_test1.h5 

# test2: Check for invalid options
TOOLTEST h5diff_2.txt -x h5diff_test1.h5 h5diff_test2.h5

# test3: Check for -h option
TOOLTEST h5diff_3.txt -h h5diff_test1.h5 h5diff_test2.h5

# test4: Check for invalid -d options
TOOLTEST h5diff_4.txt -d  h5diff_test1.h5 h5diff_test2.h5

# test5: Check for invalid -d options
TOOLTEST h5diff_5.txt -d -4  h5diff_test1.h5 h5diff_test2.h5





if test $nerrors -eq 0 ; then
   echo "All $H5DIFF tests passed."
fi

exit $nerrors
