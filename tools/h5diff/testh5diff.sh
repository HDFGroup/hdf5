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
# tests 0., Check for individual options
##############################################################################

# test 0.1: Check if the command line number of arguments is less than 3
TOOLTEST h5diff_01.txt h5diff_test1.h5 

# test 0.2: Check for invalid options
TOOLTEST h5diff_02.txt -x h5diff_test1.h5 h5diff_test2.h5

# test 0.3.1: Check for -h option
TOOLTEST h5diff_031.txt -h h5diff_test1.h5 h5diff_test2.h5

# test 0.3.3: Check for -r option
TOOLTEST h5diff_033.txt -r h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# Test -d option
##############################################################################


# test 0.4.1: no value
TOOLTEST h5diff_041.txt -d h5diff_test1.h5 h5diff_test2.h5

# test 0.4.2: negative value
TOOLTEST h5diff_042.txt -d -4 h5diff_test1.h5 h5diff_test2.h5

# test 0.4.3: zero
TOOLTEST h5diff_043.txt -d 0 h5diff_test1.h5 h5diff_test2.h5

# test 0.4.4: non number
TOOLTEST h5diff_044.txt -d u h5diff_test1.h5 h5diff_test2.h5

# test 0.4.5: hexadecimal
TOOLTEST h5diff_045.txt -d 0x1 h5diff_test1.h5 h5diff_test2.h5

# test 0.4.6: string
TOOLTEST h5diff_046.txt -d "1" h5diff_test1.h5 h5diff_test2.h5

# test 0.4.7: repeated value
TOOLTEST h5diff_047.txt -d 1 -d 2 h5diff_test1.h5 h5diff_test2.h5

# test 0.4.8: number larger than biggest difference
TOOLTEST h5diff_048.txt dset2.1a dset2.1b -d 7 h5diff_test1.h5 h5diff_test2.h5

# test 0.4.9: number smaller than smallest difference
TOOLTEST h5diff_049.txt dset2.1a dset2.1b -d 1 h5diff_test1.h5 h5diff_test2.h5

# test 0.4.10: non-integer
TOOLTEST h5diff_0410.txt dset2.1a dset2.1b -d 2.3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# Test -p option
##############################################################################


# test 0.5.1: no value
TOOLTEST h5diff_051.txt -p  h5diff_test1.h5 h5diff_test2.h5

# test 0.5.2: negative value
TOOLTEST h5diff_052.txt -p -4 h5diff_test1.h5 h5diff_test2.h5

# test 0.5.3: zero
TOOLTEST h5diff_053.txt -p 0 h5diff_test1.h5 h5diff_test2.h5

# test 0.5.4: non number
TOOLTEST h5diff_054.txt -p u h5diff_test1.h5 h5diff_test2.h5

# test 0.5.5: hexadecimal
TOOLTEST h5diff_055.txt -p 0x1 h5diff_test1.h5 h5diff_test2.h5

# test 0.5.6: string
TOOLTEST h5diff_056.txt -p "1" h5diff_test1.h5 h5diff_test2.h5

# test 0.5.7: repeated value
TOOLTEST h5diff_057.txt -p 1 -p 2 h5diff_test1.h5 h5diff_test2.h5

# test 0.5.8: number larger than biggest difference
TOOLTEST h5diff_058.txt dset2.1a dset2.1b -p 7 h5diff_test1.h5 h5diff_test2.h5

# test 0.5.9: number smaller than smallest difference
TOOLTEST h5diff_059.txt dset2.1a dset2.1b -p 1 h5diff_test1.h5 h5diff_test2.h5

# test 0.5.10: non-integer
TOOLTEST h5diff_0510.txt dset2.1a dset2.1b -p 2.3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# Test -n option
##############################################################################


# test 0.6.1: no value
TOOLTEST h5diff_061.txt -n  h5diff_test1.h5 h5diff_test2.h5

# test 0.6.2: negative value
TOOLTEST h5diff_062.txt -n -4 h5diff_test1.h5 h5diff_test2.h5

# test 0.6.3: zero
TOOLTEST h5diff_063.txt -n 0 h5diff_test1.h5 h5diff_test2.h5

# test 0.6.4: non number
TOOLTEST h5diff_064.txt -n u h5diff_test1.h5 h5diff_test2.h5

# test 0.6.5: hexadecimal
TOOLTEST h5diff_065.txt -n 0x1 h5diff_test1.h5 h5diff_test2.h5

# test 0.6.6: string
TOOLTEST h5diff_066.txt -n "1" h5diff_test1.h5 h5diff_test2.h5

# test 0.6.7: repeated value
TOOLTEST h5diff_067.txt -n 1 -n 2 h5diff_test1.h5 h5diff_test2.h5

# test 0.6.8: number larger than biggest difference
TOOLTEST h5diff_068.txt dset2.1a dset2.1b -n 7 h5diff_test1.h5 h5diff_test2.h5

# test 0.6.9: number smaller than smallest difference
TOOLTEST h5diff_069.txt dset2.1a dset2.1b -n 1 h5diff_test1.h5 h5diff_test2.h5

# test 0.6.10: non-integer
TOOLTEST h5diff_0610.txt dset2.1a dset2.1b -n 2.3 h5diff_test1.h5 h5diff_test2.h5

##############################################################################
# Test valid files
##############################################################################

# test 0.7: Check if the file names supplied are valid files
TOOLTEST h5diff_07.txt h5diff_test1.h6 h5diff_test2.h6


##############################################################################
# Check for not comparable issues
##############################################################################

##############################################################################
# Different types
##############################################################################

# test 1.1.1: Compare a dataset with a group
TOOLTEST h5diff_111.txt dset1.1 g1.1 h5diff_test1.h5 h5diff_test2.h5

# test 1.1.2 Dataset vs Link
TOOLTEST h5diff_112.txt dset1.1 soft h5diff_test1.h5 h5diff_test1.h5

# test 1.1.3 Dataset vs Named type
TOOLTEST h5diff_113.txt dset1.1 compound h5diff_test1.h5 h5diff_test1.h5

##############################################################################
# not comparable types
##############################################################################

# test 1.2.1: Group vs Group
TOOLTEST h5diff_121.txt g1.1 g1.1 h5diff_test1.h5 h5diff_test2.h5

# test 1.2.2: Type vs Type
TOOLTEST h5diff_122.txt compound h5diff_test1.h5 h5diff_test2.h5

# test 1.2.3: Link vs Link
TOOLTEST h5diff_123.txt soft soft h5diff_test1.h5 h5diff_test1.h5


##############################################################################
# Class issues
##############################################################################

# test 1.3.1: H5T_STRING
TOOLTEST h5diff_131.txt dset1.3.1 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.2: H5T_BITFIELD
TOOLTEST h5diff_132.txt dset1.3.2 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.3: H5T_OPAQUE
TOOLTEST h5diff_133.txt dset1.3.3 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.4: H5T_COMPOUND
TOOLTEST h5diff_134.txt dset1.3.4 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.5: H5T_REFERENCE
TOOLTEST h5diff_135.txt dset1.3.5 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.6: H5T_ENUM
TOOLTEST h5diff_136.txt dset1.3.6 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.7: H5T_VLEN
TOOLTEST h5diff_137.txt dset1.3.7 h5diff_test1.h5 h5diff_test1.h5

# test 1.3.8: H5T_ARRAY
TOOLTEST h5diff_138.txt dset1.3.8 h5diff_test1.h5 h5diff_test1.h5


# test 1.4: Compare integer with float
TOOLTEST h5diff_14.txt dset1.1 dset1.4 h5diff_test1.h5 h5diff_test2.h5

# test 1.5 : Check for the same rank, for datasets
TOOLTEST h5diff_15.txt dset1.1 dset1.5 h5diff_test1.h5 h5diff_test2.h5

# test 1.6: Check for the same current dimensions. Only compare if they are the same.
TOOLTEST h5diff_16.txt dset1.1 dset1.6 h5diff_test1.h5 h5diff_test2.h5

# test 1.7: Check for the same maximum dimensions. Give a warning if they are different. 
TOOLTEST h5diff_17.txt dset1.7 dset1.7 h5diff_test1.h5 h5diff_test2.h5

# test 1.8: Check for the same storage datatype. Give a warning if they are different. 
TOOLTEST h5diff_18.txt dset1.8 dset1.8 h5diff_test1.h5 h5diff_test2.h5


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
