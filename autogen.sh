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

# A script to reconfigure the Autotools for HDF5, and to recreate other
# generated files specific to HDF5.
#
# IMPORTANT OS X NOTE
#
# If you are using OS X, you will probably not have the Autotools
# installed, even if you have the Xcode command-line tools.
#
# The easiest way to fix this is to install everything via Homebrew:
#
#   http://brew.sh/
#
# After you install the base packages, install autoconf, automake,
# and libtool.
#
#   brew install autoconf
#   brew install automake
#   brew install libtool
#
# END IMPORTANT OS X NOTE

echo
echo "**************************"
echo "* HDF5 autogen.sh script *"
echo "**************************"
echo

# Run scripts that process source.
#
# These should be run before the Autotools so that failures here block
# compilation.

# Run trace script
# The trace script adds H5TRACE macros to library source files.  It should
# have no effect on files that don't have HDF5 API macros in them.
echo "Running trace script:"
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

# Run Autotools

# The "obsolete" warnings category flags our Java macros as obsolete.
# Since there is no clear way to upgrade them (Java support in the Autotools
# is not great) and they work well enough for now, we suppress those warnings.
echo "Running Autotools"
echo
echo "NOTE: You can ignore the warning about adding -I m4."
echo "      We already do this in an included file."
echo
autoreconf -vif --warnings=no-obsolete || exit 1
echo

echo "*** SUCCESS ***"

echo
exit 0
