#! /bin/sh
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# Run the VOL-independent tools tests

echo
echo "********************"
echo "* TESTING: h5mkgrp *"
echo "********************"
echo

sh testh5mkgrp.sh

echo
echo "*****************"
echo "* TESTING: h5ls *"
echo "*****************"
echo

sh testh5ls.sh

echo
echo "*******************"
echo "* TESTING: h5dump *"
echo "*******************"
echo

sh testh5dump.sh

echo
echo "*********************"
echo "* TESTING: h5repack *"
echo "*********************"
echo

sh testh5repack.sh

echo
echo "*******************"
echo "* TESTING: h5diff *"
echo "*******************"
echo

sh testh5diff.sh

echo
echo "*******************"
echo "* TESTING: h5copy *"
echo "*******************"
echo

sh testh5copy.sh

echo
echo "********"
echo "* DONE *"
echo "********"
echo
