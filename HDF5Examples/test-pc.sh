#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

# This file is for use of h5cc created with the CMake process.
# Environment variable, HDF5_HOME is expected to be set.
# $1 is the path name of the source directory.
# $2 is the path name of the build directory.
# $3 is the current path name.

top_srcdir=$1
top_builddir=$2
currentpath=.
verbose=yes
nerrors=0

return_val=0

mkdir -p "$top_builddir"
cd "$top_srcdir"
# Loop through all subdirectories
for dir in */; do
  if [ -d "$dir" ] && [ ! -L "$dir" ]; then
    # Check if test-pc.sh exists
    if [ -f "$dir/test-pc.sh" ];
    then
        echo "Entering directory: $dir"
        (
            mkdir -p "$top_builddir/$dir"
            cd "$dir"
            ./test-pc.sh $top_srcdir/ $top_builddir/ $dir # Execute script in the subdirectory
            status=$?
            exit $status
        )
        return_val=$?
        if test $return_val -ne 0
        then
          echo "Exiting directory: $dir with $return_val tests FAILED"
        else
          echo "Exiting directory: $dir Passed"
        fi
        nerrors=`expr $return_val + $nerrors`
    fi
  fi
done

echo "$nerrors tests failed in HDF5Examples"
exit $nerrors
