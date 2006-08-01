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
# Pedro Vicente Nunes
# pvn@hdfgroup.org
# Thursday, July 20, 2006
#

H5COPY=h5copy               # The tool name
H5COPY_BIN=`pwd`/$H5COPY    # The path of the tool binary

nerrors=0

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi
test -d ../testfiles || mkdir ../testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() {
	SPACES="                                                               "
	    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Run a test and print PASS or *FAIL*. If h5copy can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable.

TOOLTEST() {
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
}

##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################

TOOLTEST -v $srcdir/../testfiles/test1.h5/array test1.out.h5/array
TOOLTEST -v $srcdir/../testfiles/test1.h5/integer test1.out.h5/integer_copy
TOOLTEST -v $srcdir/../testfiles/test1.h5/g1 test1.out.h5/g1

if test $nerrors -eq 0 ; then
    echo "All h5copy tests passed."
fi

exit $nerrors
