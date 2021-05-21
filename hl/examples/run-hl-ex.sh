#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

#
#  This file:  run-hl-ex.sh
# Written by:  Larry Knox
#       Date:  May 11, 2010
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                               #
# This script will run the scripts to compile and run the hdf5 hl examples.     #
#                                                                               #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

if (echo "Run hl c examples" && \
   (cd c; sh ./run-hlc-ex.sh) && \
   (if test -d fortran; then
       echo "Run hl fortran examples" 
       cd fortran; sh ./run-hlfortran-ex.sh
    fi) 
   (if test -d c++; then 
       echo "Run hl c++ examples"
       cd c++; sh ./run-hlc++-ex.sh
    fi)); then
   echo "Finished running hl examples"
   exit 0
else
   exit 1
fi  
