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
# Tests for the h52jpeg tool
# Pedro Vicente Nunes (THG), 6/16/2008

TOOL=h52jpeg               # The tool name
TOOL_BIN=`pwd`/$TOOL       # The path of the tool binary

SRCFILE=h52jpegtst.h5
INDIR=$srcdir/../testfiles
TESTFILE="$INDIR/$SRCFILE"

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

# Just call the tool binary with the command line parameters
#
TOOLTEST() 
{
    # Run test.
    # Tflops interprets "$@" as "" when no parameter is given (e.g., the
    # case of missing file name).  Changed it to use $@ till Tflops fixes it.
    
    if [ "`uname -s`" = "TFLOPS O/S" ]; then
     $RUNSERIAL $TOOL_BIN $@ 
    else
     $RUNSERIAL $TOOL_BIN "$@" 
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
# The tests 
# To avoid the printing of the complete full path of the test file, that hides
# all the other parameters for long paths, the printing of the command line 
# is done first in
# TESTING with the name only of the test file $TOOL, not its full path $TESTFILE
##############################################################################


# Test for traversing the file and export all images/datasets to jpeg
TESTING  $TOOL $SRCFILE myjpeg
TOOLTEST $TESTFILE myjpeg 



if test $nerrors -eq 0 ; then
   echo "All $TOOL tests passed."
fi

exit $nerrors

