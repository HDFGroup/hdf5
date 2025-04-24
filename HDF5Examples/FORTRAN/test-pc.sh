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

# This file is for use of h5cc created with the CMake process
# HDF5_HOME is expected to be set

srcdir=..
builddir=.
verbose=yes
nerrors=0

# Loop through all subdirectories
for dir in */; do
  if [ -d "$dir" ]; then
    #check if sysconfig.out exists
    if [ -f "$dir/test-pc.sh" ];
    then
        echo "Entering directory: $dir"
        (
          cd "$dir" && mkdir "build"
          cd "build" && ../test-pc.sh # Execute script in the subdirectory
        )
        echo "Exiting directory: $dir"
    fi
  fi
done