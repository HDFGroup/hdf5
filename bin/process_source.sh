#!/bin/sh
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
#

# A convenience script to process HDF5 source. This recreates some header files
# from their input files and runs the trace script to update the H5ARG_TRACE
# macros.
#
# This does NOT regenerate the parser code in the high-level library, since
# that would probably generate a lot of churn due to different flex, etc.
# versions. If you want to regenerate that code, use the genparser script
# from the bin directory.

echo
echo "******************************"
echo "* HDF5 process source script *"
echo "******************************"
echo
echo "*** NOTE: Must be run from the source root! ***"
echo

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
