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
    if test -z "$HDF5_NOCLEANUP"; then
    rm -f $actual $actual_err
    fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                            ###
##############################################################################
##############################################################################

##############################################################################
# tests 0., check for bad input values
##############################################################################

# test 0.1: Check if the command line number of arguments is less than 3
TOOLTEST h5diff_01.txt h5diff_test1.h5 

# test 0.2: Check for invalid options
TOOLTEST h5diff_02.txt -x h5diff_test1.h5 h5diff_test2.h5

# test 0.3: Check for -h option
TOOLTEST h5diff_03.txt -h h5diff_test1.h5 h5diff_test2.h5

# test 0.4: Check for invalid -d options
TOOLTEST h5diff_04.txt -d  h5diff_test1.h5 h5diff_test2.h5

# test 0.5: Check for invalid -d options
TOOLTEST h5diff_05.txt -d -4  h5diff_test1.h5 h5diff_test2.h5

# test 0.6: Check for invalid -p options
TOOLTEST h5diff_06.txt -p  h5diff_test1.h5 h5diff_test2.h5

# test 0.7: Check for invalid -p options
TOOLTEST h5diff_07.txt -p -4  h5diff_test1.h5 h5diff_test2.h5

# test 0.8: Check for invalid -n options
TOOLTEST h5diff_08.txt -n  h5diff_test1.h5 h5diff_test2.h5

# test 0.9: Check for invalid -n options
TOOLTEST h5diff_09.txt -n 0  h5diff_test1.h5 h5diff_test2.h5

# test 0.10: Check if the file names supplied are valid files
TOOLTEST h5diff_010.txt h5diff_test1.h6 h5diff_test2.h6

##############################################################################
# tests 1., Check for not comparable issues
##############################################################################

# test 1.1.1: Objects are not the same type (e.g try to compare a group with a dataset)
TOOLTEST h5diff_111.txt dset1.1 g1.1 h5diff_test1.h5 h5diff_test2.h5

# test 1.1.2: Objects are not the same type (e.g try to compare a group with a dataset)
TOOLTEST h5diff_112.txt g1.1 g1.1 h5diff_test1.h5 h5diff_test2.h5

# test 1.2.1: Objects are of classes H5G_TYPE and H5G_GROUP and their name is supplied
TOOLTEST h5diff_121.txt compound h5diff_test1.h5 h5diff_test2.h5

# test 1.2.2: Objects are of classes H5G_TYPE and H5G_GROUP and their name is supplied
TOOLTEST h5diff_122.txt enum h5diff_test1.h5 h5diff_test2.h5

# test 1.3: Check for non supported classes. Supported classes are H5T_INTEGER and H5T_FLOAT
TOOLTEST h5diff_13.txt dset1.3 h5diff_test1.h5 h5diff_test2.h5

# test 1.4: Objects are not the same dataset class type
TOOLTEST h5diff_14.txt dset1.1 dset1.4 h5diff_test1.h5 h5diff_test2.h5

# test 1.5: Check for the same rank, for datasets
TOOLTEST h5diff_15.txt dset1.1 dset1.5 h5diff_test1.h5 h5diff_test2.h5

# test 1.6: Check for the same current dimensions
TOOLTEST h5diff_16.txt dset1.1 dset1.6 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# tests 2., Different datatype sizes and different mix of options 
##############################################################################

##############################################################################
# H5T_INTEGER size 1 
##############################################################################

# test 2.1.0
TOOLTEST h5diff_210.txt dset2.1 dset2.2 h5diff_test1.h5 h5diff_test2.h5

# test 2.1.1
TOOLTEST h5diff_211.txt dset2.1 dset2.2 -n 2 h5diff_test1.h5 h5diff_test2.h5

# test 2.1.2
TOOLTEST h5diff_212.txt dset2.1 dset2.2 -d 3 h5diff_test1.h5 h5diff_test2.h5

# test 2.1.3
TOOLTEST h5diff_213.txt dset2.1a dset2.1b -p 3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# H5T_INTEGER size 2
##############################################################################

# test 2.2.0
TOOLTEST h5diff_220.txt dset2.2a dset2.2b h5diff_test1.h5 h5diff_test2.h5
# test 2.2.1
TOOLTEST h5diff_221.txt dset2.2a dset2.2b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.2.2
TOOLTEST h5diff_222.txt dset2.2a dset2.2b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.2.3
TOOLTEST h5diff_223.txt dset2.2a dset2.2b -p 3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# H5T_INTEGER size 4
##############################################################################

# test 2.3.0
TOOLTEST h5diff_230.txt dset2.3a dset2.3b h5diff_test1.h5 h5diff_test2.h5
# test 2.3.1
TOOLTEST h5diff_231.txt dset2.3a dset2.3b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.3.2
TOOLTEST h5diff_232.txt dset2.3a dset2.3b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.3.3
TOOLTEST h5diff_233.txt dset2.3a dset2.3b -p 3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# H5T_INTEGER size 8
##############################################################################

# test 2.4.0
TOOLTEST h5diff_240.txt dset2.4a dset2.4b h5diff_test1.h5 h5diff_test2.h5
# test 2.4.1
TOOLTEST h5diff_241.txt dset2.4a dset2.4b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.4.2
TOOLTEST h5diff_242.txt dset2.4a dset2.4b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.4.3
TOOLTEST h5diff_243.txt dset2.4a dset2.4b -p 3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# H5T_FLOAT size 4
##############################################################################

# test 2.5.0
TOOLTEST h5diff_250.txt dset2.5a dset2.5b h5diff_test1.h5 h5diff_test2.h5
# test 2.5.1
TOOLTEST h5diff_251.txt dset2.5a dset2.5b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.5.2
TOOLTEST h5diff_252.txt dset2.5a dset2.5b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.5.3
TOOLTEST h5diff_253.txt dset2.5a dset2.5b -p 3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# H5T_FLOAT size 8
##############################################################################

# test 2.6.0
TOOLTEST h5diff_260.txt dset2.6a dset2.6b h5diff_test1.h5 h5diff_test2.h5
# test 2.6.1
TOOLTEST h5diff_261.txt dset2.6a dset2.6b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.6.2
TOOLTEST h5diff_262.txt dset2.6a dset2.6b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.6.3
TOOLTEST h5diff_263.txt dset2.6a dset2.6b -p 3 h5diff_test1.h5 h5diff_test2.h5


#######################################################
# Different combination of objects
#######################################################

# test 3.0
TOOLTEST h5diff_30.txt h5diff_test3.h5 h5diff_test4.h5

# test 3.1
TOOLTEST h5diff_31.txt dset3 dset3 h5diff_test3.h5 h5diff_test4.h5

# test 3.2
TOOLTEST h5diff_32.txt dset3 dset4 h5diff_test3.h5 h5diff_test4.h5

# test 3.3
TOOLTEST h5diff_33.txt dset6 dset3 h5diff_test3.h5 h5diff_test4.h5

# test 3.4
TOOLTEST h5diff_34.txt dset6 dset6 h5diff_test3.h5 h5diff_test4.h5




if test $nerrors -eq 0 ; then
   echo "All $H5DIFF tests passed."
fi

exit $nerrors
