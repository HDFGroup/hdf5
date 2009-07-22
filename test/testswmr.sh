#! /bin/sh -x
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
# Tests for the swmr feature.
#
# Created:
#   Albert Cheng, 2009/07/22


###############################################################################
## test parameters
###############################################################################

Nreaders=5		# number of readers to launch
Nrecords=1000000	# number of records to write
Nsecs=5			# number of seconds per read interval
nerrors=0

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

# Parse option
#   -p   run ph5diff tests
#   -h   print help page
while [ $# -gt 0 ]; do
    case "$1" in
    *)  # unknown option
        echo "$0: Unknown option ($1)"
	exit 1
	;;
    esac
done

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

./swmr_generator
echo launch the swmr_writer
./swmr_writer $Nrecords &
# launch readers
n=0
echo launch $Nreaders swmr_readers
while [ $n -lt $Nreaders ]; do
    ./swmr_reader $Nsecs &
    n=`expr $n + 1`
done
wait

# ##############################################################################
# # END
# ##############################################################################

if test $nerrors -eq 0 ; then
   echo "SWMR tests passed."
fi

exit $nerrors
