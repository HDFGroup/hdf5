#!/bin/sh

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
