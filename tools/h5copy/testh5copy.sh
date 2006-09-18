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
# Tests for the h5copy tool
#
# Pedro Vicente Nunes (pvn@hdfgroup.org), Albert Cheng (acheng@hdfgroup.org)
# Thursday, July 20, 2006
#

H5COPY=h5copy               # The tool name
H5COPY_BIN=`pwd`/$H5COPY    # The path of the tool binary
H5DIFF=../h5diff/h5diff     # The h5diff tool name 
H5DIFF_BIN=`pwd`/$H5DIFF    # The path of the h5diff  tool binary

TESTFILE=$srcdir/../testfiles/h5copytst.h5
FILEOUT=h5copytst.out.h5

nerrors=0

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi
test -d ../testfiles || mkdir ../testfiles

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
    ) > output.out
    RET=$?
    if [ $RET != 0 ] ; then
    echo "*FAILED*"
    echo "failed result is:"
    cat output.out
    nerrors="`expr $nerrors + 1`"
    else
    echo " PASSED"
    fi
    
    if [ $runh5diff != no ]; then
     H5DIFFTEST $inputfile $outputfile 
    fi
}

# Call the h5diff tool
#
H5DIFFTEST() 
{
    VERIFY  $@
    if [ "`uname -s`" = "TFLOPS O/S" ]; then
     $RUNSERIAL $H5DIFF_BIN $@ -q
    else
     $RUNSERIAL $H5DIFF_BIN "$@" -q
    fi
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi
}

##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################

TOOLTEST -i $TESTFILE -o $FILEOUT -v -s simple -d simple
TOOLTEST -i $TESTFILE -o $FILEOUT -v -s chunk -d chunk



if test $nerrors -eq 0 ; then
    echo "All h5copy tests passed."
fi

exit $nerrors



