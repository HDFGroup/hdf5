#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

#
#  This file:  run-hl-ex.sh
# Written by:  Larry Knox
#       Date:  May 11, 2010
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                               #
# This script will run the scripts to compile and run the installed hdf5        #
# examples.                                                                     #
#                                                                               #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

echo "Run c examples"
if ((cd c; sh ./run-c-ex.sh) && \
   (if test -d fortran; then   
       echo "Run fortran examples" 
       cd fortran; sh ./run-fortran-ex.sh 
    fi) 
   (if test -d c++; then
       echo "Run c++ examples" 
       cd c++; sh ./run-c++-ex.sh
    fi)
   (if test -d hl; then
       echo "Run hl examples." 
       cd hl; sh ./run-hl-ex.sh
    fi)); then
   echo "Done"
   exit 0
else
   exit 1
fi

