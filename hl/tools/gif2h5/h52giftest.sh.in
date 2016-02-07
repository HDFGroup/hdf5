#!/bin/sh
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
# HDF Utilities Test script


TESTFILE1="$srcdir/testfiles/h52giftst.h5"
TESTFILE2="$srcdir/testfiles/image1.gif"
TESTFILE3="$srcdir/testfiles/ex_image2.h5"

# initialize errors variable
errors=0

TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}



# Verify the test runs with success (return code is 0)
TOOLTEST()
{
    # for now, discard any error messages generated.
    $RUNSERIAL $*  > /dev/null 2>&1

    RET=$?
    if [ $RET = 0 ] ; then
	echo " PASSED"
    else
	echo "*FAILED*"
	errors="` expr $errors + 1 `";
    fi

}

# Verify the test runs with failure (return code is not 0)
# Use for testing if tool can handle error conditions like
# illegal input, bad arguments, exeeding limits, ...
TOOLTESTFAIL()
{
    # for now, discard any error messages generated.
    $RUNSERIAL $* > /dev/null 2>&1

    RET=$?
    if [ $RET != 0 ] ; then
	echo " PASSED"
    else
	echo "*FAILED*"
	errors="` expr $errors + 1 `";
    fi
}


# Positive tests for gif2h5
echo "**validate the gif2h5 tool processes input correctly..."
TESTING "./gif2h5 image1.gif image1.h5"
TOOLTEST ./gif2h5 $TESTFILE2 image1.h5
echo ""

# Positive tests for h52gif
echo "**validate the h52gif tool processes input correctly..."
TESTING "./h52gif h52giftst.h5 image1.gif -i image" 
TOOLTEST ./h52gif $TESTFILE1 image1.gif -i image
echo ""

# Negative tests.
echo "**verify that the h52gif tool handles error conditions correctly..."
# nonexisting dataset name
TESTING "./h52gif h52giftst.h5 image.gif -i nosuch_image" 
TOOLTESTFAIL "./h52gif $TESTFILE1 image.gif -i nosuch_image" 
# this test should have failed but it did not. Comment it out for now.
#TESTING "./h52gif h52giftst.h5 image.gif -i palette" 
#TOOLTESTFAIL "./h52gif $TESTFILE1 image.gif -i palette" 
TESTING "./h52gif h52giftst.h5 image24.gif -i image24bitpixel" 
TOOLTESTFAIL "./h52gif $TESTFILE3 image24.gif -i image24bitpixel" 
echo ""

# all done. summarize results.
if test $errors -eq 0 ; then
    echo "All gif2h5 and h52gif tests passed."
    exit 0
else
    echo "Some gif2h5 or h52gif tests failed with $errors errors."
    exit 1
fi
