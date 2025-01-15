#!/bin/sh
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

# A script to recreate other
# generated files specific to HDF5.
#

echo
echo "**************************"
echo "* HDF5 autogen.sh script *"
echo "**************************"
echo

# Run scripts that process source.

# Run trace script
# The trace script updates H5ARG_TRACE macros in library source files.
echo "Running arg trace script:"
bin/trace src/H5*.c || exit 1
echo

# Run make_err
# make_err automatically generates the H5E headers that create error message
# types for HDF5.
echo "Running error generation script:"
bin/make_err src/H5err.txt || exit 1
echo

# Run make_vers
# make_vers automatically generates the public headers that define the API version
# macros for HDF5.
echo "Running API version generation script:"
bin/make_vers src/H5vers.txt || exit 1
echo

# Run make_overflow
# make_overflow automatically generates macros for detecting overflows for type
# conversion.
echo "Running overflow macro generation script:"
bin/make_overflow src/H5overflow.txt || exit 1
echo

echo "*** SUCCESS ***"

echo
exit 0
