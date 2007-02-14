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

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

actual=sample.out
expect=$srcdir/expected.out

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING()
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

TESTING C++ Examples

(
   ./create
   ./readdata
   ./writedata
   ./compound
   ./extend_ds
   ./chunks
   ./h5group
) > $actual

if $CMP $expect $actual; then
   echo " PASSED"
else
   echo "*FAILED*"
   echo "    Expected result differs from actual result"
   nerrors="`expr $nerrors + 1`"
   test yes = "$verbose" && $DIFF $expect $actual | sed 's/^/    /'
fi

# Clean up output file
if test -z "$HDF5_NOCLEANUP"; then
   rm -f $actual
fi

if test $nerrors -eq 0 ; then
   echo "All tests passed."
fi

exit $nerrors
