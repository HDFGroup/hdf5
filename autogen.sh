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

# A script to reconfigure autotools for HDF5, and to recreate other
# generated files specific to HDF5.
#
# IMPORTANT OS X NOTE
#
# If you are using OS X, you will probably not have the autotools
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
# This only takes a few minutes. Note that libtool and libtoolize will
# be glibtool and glibtoolize so as not to conflict with Apple's non-gnu
# tools. This autogen.sh script handles this for you.
#
# END IMPORTANT OS X NOTE
#
# If you want to use a particular version of the autotools, the paths
# to each tool can be overridden using the following environment
# variables:
#
#   HDF5_ACLOCAL
#   HDF5_AUTOHEADER
#   HDF5_AUTOMAKE
#   HDF5_AUTOCONF
#   HDF5_LIBTOOLIZE
#   HDF5_M4
#
# Note that aclocal will attempt to include libtool's share/aclocal
# directory.
#
# Aside from -h for help, this script takes one potential option:
#
# -v
#
# This emits some extra information, mainly tool versions.

echo
echo "**************************"
echo "* HDF5 autogen.sh script *"
echo "**************************"
echo

# Default is not verbose output
verbose=false

optspec=":hpv-"
while getopts "$optspec" optchar; do
    case "${optchar}" in
    h)
        echo "usage: $0 [OPTIONS]"
        echo
        echo "      -h      Print this help message."
        echo
        echo "      -v      Show more verbose output."
        echo
        echo "  NOTE: Each tool can be set via an environment variable."
        echo "        These are documented inside this autogen.sh script."
        echo
        exit 0
        ;;
    v)
        echo "Setting verbosity: high"
        echo
        verbose=true
        ;;
    *)
        if [ "$OPTERR" != 1 ] || case $optspec in :*) ;; *) false; esac; then
            echo "ERROR: non-option argument: '-${OPTARG}'" >&2
            echo "Quitting"
            exit 1
        fi
        ;;
    esac
done

# If paths to autotools are not specified, use whatever the system
# has installed as the default. We use 'command -v <tool>' to
# show exactly what's being used (shellcheck complains that 'which'
# is non-standard and deprecated).
if test -z "${HDF5_AUTOCONF}"; then
    HDF5_AUTOCONF="$(command -v autoconf)"
fi
if test -z "${HDF5_AUTOMAKE}"; then
    HDF5_AUTOMAKE="$(command -v automake)"
fi
if test -z "${HDF5_AUTOHEADER}"; then
    HDF5_AUTOHEADER="$(command -v autoheader)"
fi
if test -z "${HDF5_ACLOCAL}"; then
    HDF5_ACLOCAL="$(command -v aclocal)"
fi
if test -z "${HDF5_LIBTOOLIZE}"; then
    # check for glibtoolize (likely found on MacOS). If not found - check for libtoolize
    HDF5_LIBTOOLIZE="$(command -v glibtoolize)"
    if [ ! -f "$HDF5_LIBTOOLIZE" ] ; then
        HDF5_LIBTOOLIZE="$(command -v libtoolize)"
    fi
fi
if test -z "${HDF5_M4}"; then
    HDF5_M4="$(command -v m4)"
fi


# Make sure that these versions of the autotools are in the path
AUTOCONF_DIR=$(dirname "${HDF5_AUTOCONF}")
LIBTOOL_DIR=$(dirname "${HDF5_LIBTOOLIZE}")
M4_DIR=$(dirname "${HDF5_M4}")
PATH=${AUTOCONF_DIR}:${LIBTOOL_DIR}:${M4_DIR}:$PATH

# Run scripts that process source.
#
# These should be run before the autotools so that failures here block
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

# Run autotools in order
#
# When available, we use the --force option to ensure all files are
# updated. This prevents the autotools from re-running to fix dependencies
# during the 'make' step, which can be a problem if environment variables
# were set on the command line during autogen invocation.

# Some versions of libtoolize will suggest that we add ACLOCAL_AMFLAGS
# = '-I m4'. This is already done in commence.am, which is included
# in Makefile.am. You can ignore this suggestion.

# LIBTOOLIZE
libtoolize_cmd="${HDF5_LIBTOOLIZE} --copy --force"
echo "${libtoolize_cmd}"
if [ "$verbose" = true ] ; then
    ${HDF5_LIBTOOLIZE} --version
fi
${libtoolize_cmd} || exit 1
echo
echo "NOTE: You can ignore the warning about adding -I m4."
echo "      We already do this in an included file."
echo

# ACLOCAL
if test -e "${LIBTOOL_DIR}/../share/aclocal" ; then
    aclocal_include="-I ${LIBTOOL_DIR}/../share/aclocal"
fi
aclocal_cmd="${HDF5_ACLOCAL} --force -I m4 ${aclocal_include}"
echo "${aclocal_cmd}"
if [ "$verbose" = true ] ; then
    ${HDF5_ACLOCAL} --version
fi
${aclocal_cmd} || exit 1
echo

# AUTOHEADER
autoheader_cmd="${HDF5_AUTOHEADER} --force"
echo "${autoheader_cmd}"
if [ "$verbose" = true ] ; then
    ${HDF5_AUTOHEADER} --version
fi
${autoheader_cmd} || exit 1
echo

# AUTOMAKE
automake_cmd="${HDF5_AUTOMAKE} --copy --add-missing --force-missing"
echo "${automake_cmd}"
if [ "$verbose" = true ] ; then
    ${HDF5_AUTOMAKE} --version
fi
${automake_cmd} || exit 1
echo

# AUTOCONF
# The "obsolete" warnings category flags our Java macros as obsolete.
# Since there is no clear way to upgrade them (Java support in the Autotools
# is not great) and they work well enough for now, we suppress those warnings.
autoconf_cmd="${HDF5_AUTOCONF} --force --warnings=no-obsolete"
echo "${autoconf_cmd}"
if [ "$verbose" = true ] ; then
    ${HDF5_AUTOCONF} --version
fi
${autoconf_cmd} || exit 1
echo

echo "*** SUCCESS ***"

echo
exit 0

