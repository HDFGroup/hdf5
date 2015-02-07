#! /bin/sh
#
# Copyright by The HDF Group.                                              
# All rights reserved.                                                     
#                                                                          
# This file is part of HDF5. The full HDF5 copyright notice, including     
# terms governing use, modification, and redistribution, is contained in   
# the files COPYING and Copyright.html.  COPYING can be found at the root  
# of the source code distribution tree; Copyright.html can be found at the 
# root level of an installed copy of the electronic document set and is    
# linked from the top-level documents page.  It can also be found at       
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have access  
# to either file, you may request a copy from help@hdfgroup.org.           
#

# A script to reconfigure autotools for HDF5, and to recreate other
# generated files specifc to HDF5.
#
# IMPORTANT OS-X NOTE
#
# If you are using OS-X, you will probably not have the autotools
# installed, even if you have the XCode command-line tools. The
# easiest way to fix this is to install them via Homebrew:
#
#   http://brew.sh/
#
# After you install the base packages, install autoconf, automake, and
# libtool.
#
#   brew install autoconf
#   brew install automake
#   brew install libtool
#
# This only takes a few minutes. Note that libtool and libtoolize will
# be glibtool and glibtoolize so as not to conflict with Apple's non-gnu
# tools. This autogen.sh script handles this for you.
#
# END IMPORTANT OS-X NOTE
#
# If you want to use a particular version of the autotools, the paths
# to each tool can be overridden using the following environment
# variables:
#
#   HDF5_ACLOCAL
#   HDF5_AUTOHEADER
#   HDF5_AUTOMAKE
#   HDF5_AUTOCONF
#   HDF5_LIBTOOL
#   HDF5_M4
#   HDF5_FLEX
#   HDF5_BISON
#
# Note that aclocal will attempt to include libtool's share/aclocal
# directory.
#
# This script takes two potential options:
#
# -p, --production
#
# When this is selected, the autotools versions are set to the paths
# and versions used by The HDF Group to produce the released versions
# of the library.
#
# NOTE: This is probably temporary. Once we update our dev machines
# to have recent versions of the autotools this option will probably
# be removed.
#
# -v, --verbose
#
# This emits some extra information, mainly tool versions.

echo
echo "**************************"
echo "* HDF5 autogen.sh script *"
echo "**************************"
echo

# Default is not production
production=false

# Default is not verbose output
verbose=false

optspec=":hpv-"
while getopts "$optspec" optchar; do
    case "${optchar}" in
    -)
        case "${OPTARG}" in
            production)
                echo "Setting production mode..."
                echo
                production=true
                ;;
            verbose)
                echo "Setting verbosity: high"
                echo
                verbose=true
                ;;
            *)
                if [ "$OPTERR" = 1 ] && [ "${optspec:0:1}" != ":" ]; then
                    echo "Unknown option --${OPTARG}" >&2
                fi
                ;;
        esac;;
    h)
        echo "usage: $0 [-p|--production]"
        echo
        echo "      -p      Used by THG to ensure that particular versions"
        echo "              of the autotools are used and hard-codes"
        echo "              autotools paths to THG machines. Not for"
        echo "              non-HDF-Group users!"
        echo
        echo "  NOTE: Each autotool can be set via an environment variable."
        echo "        These are documented inside this autogen.sh script."
        echo
        exit 0
        ;;
    p)
        echo "Setting production mode..."
        production=true
        ;;
    v)
        echo "Setting verbosity: high"
        verbose=true
        ;;
    *)
        if [ "$OPTERR" != 1 ] || [ "${optspec:0:1}" = ":" ]; then
            echo "Non-option argument: '-${OPTARG}'" >&2
        fi
        ;;
    esac
done

if [ "$production" = true ] ; then

    # Production mode
    #
    # Hard-code canonical HDF Group tool locations and ensure
    # version numbers are correct.

    # Production versions of the tools
    AUTOCONF_VERSION="autoconf (GNU Autoconf) 2.69"
    AUTOMAKE_VERSION="automake (GNU automake) 1.14.1"
    AUTOHEADER_VERSION="autoheader (GNU Autoconf) 2.69"
    ACLOCAL_VERSION="aclocal (GNU automake) 1.14.1"
    LIBTOOL_VERSION="(GNU libtool) 2.4.2"
    M4_VERSION="m4 (GNU M4) 1.4.17"
    BISON_VERSION="bison (GNU Bison) 2.7"
    FLEX_VERSION="flex 2.5.37"

    # If paths to tools are not specified, assume they are
    # located in /mnt/hdf/packages and set paths accordingly.
    if test -z ${HDF5_AUTOCONF}; then
        HDF5_AUTOCONF=/mnt/hdf/packages/autoconf/autoconf-2.69/bin/autoconf
    fi
    if test -z ${HDF5_AUTOMAKE}; then
        HDF5_AUTOMAKE=/mnt/hdf/packages/automake/automake-1.14.1/bin/automake-1.14
    fi
    if test -z ${HDF5_AUTOHEADER}; then
        HDF5_AUTOHEADER=/mnt/hdf/packages/autoconf/autoconf-2.69/bin/autoheader
    fi
    if test -z ${HDF5_ACLOCAL}; then
        HDF5_ACLOCAL=/mnt/hdf/packages/automake/automake-1.14.1/bin/aclocal-1.14
    fi
    if test -z ${HDF5_LIBTOOL}; then
        HDF5_LIBTOOL=/mnt/hdf/packages/libtool/libtool-2.4.2/bin/libtool
    fi
    if test -z ${HDF5_M4}; then
        HDF5_M4=/mnt/hdf/packages/m4/m4-1.4.17/bin/m4
    fi
    if test -z ${HDF5_BISON}; then
        HDF5_BISON=/usr/hdf/bin/bison
    fi
    if test -z ${HDF5_FLEX}; then
        HDF5_FLEX=/usr/hdf/bin/flex
    fi

    # Check version numbers of all autotools against the "correct" versions
    AC_VERS=`${HDF5_AUTOCONF} --version 2>&1 | grep "^${AUTOCONF_VERSION}"`
    if test -z "${AC_VERS}"; then
        echo "${HDF5_AUTOCONF} version is not ${AUTOCONF_VERSION}"
        ${HDF5_AUTOCONF} --version
        exit 1
    fi
    AM_VERS=`${HDF5_AUTOMAKE} --version 2>&1 | grep "^${AUTOMAKE_VERSION}"`
    if test -z "${AM_VERS}"; then
       echo "${HDF5_AUTOMAKE} version is not ${AUTOMAKE_VERSION}"
       ${HDF5_AUTOMAKE} --version
       exit 1
    fi
    AH_VERS=`${HDF5_AUTOHEADER} --version 2>&1 | grep "^${AUTOHEADER_VERSION}"`
    if test -z "${AH_VERS}"; then
        echo "${HDF5_AUTOHEADER} version is not ${AUTOHEADER_VERSION}"
        ${HDF5_AUTOHEADER} --version
        exit 1
    fi
    AL_VERS=`${HDF5_ACLOCAL} --version 2>&1 | grep "^${ACLOCAL_VERSION}"`
    if test -z "${AL_VERS}"; then
        echo "${HDF5_ACLOCAL} version is not ${ACLOCAL_VERSION}"
        ${HDF5_ACLOCAL} --version
        exit 1
    fi
    LT_VERS=`${HDF5_LIBTOOL} --version 2>&1 | grep "${LIBTOOL_VERSION}"`
    if test -z "${LT_VERS}"; then
        echo "${HDF5_LIBTOOL} version is not ${LIBTOOL_VERSION}"
        ${HDF5_LIBTOOL} --version
        exit 1
    fi
    M4_VERS=`${HDF5_M4} --version 2>&1 | grep "${M4_VERSION}"`
    if test -z "${M4_VERS}"; then
        echo "${HDF5_M4} version is not ${M4_VERSION}"
        ${HDF5_M4} --version
        exit 1
    fi
    BI_VERS=`${HDF5_BISON} --version 2>&1 | grep "^${BISON_VERSION}"`
    if test -z "${BI_VERS}"; then
       echo "${HDF5_BISON} version is not ${BISON_VERSION}"
       exit 1
    fi
    FL_VERS=`${HDF5_FLEX} --version 2>&1 | grep "^${FLEX_VERSION}"`
    if test -z "${FL_VERS}"; then
       echo "${HDF5_FLEX} version is not ${FLEX_VERSION}"
       exit 1
    fi

else

    # Not in production mode
    #
    # If paths to autotools are not specified, use whatever the system
    # has installed as the default. We use 'which <tool>' to
    # show exactly what's being used.
    if test -z ${HDF5_AUTOCONF}; then
        HDF5_AUTOCONF=$(which autoconf)
    fi
    if test -z ${HDF5_AUTOMAKE}; then
        HDF5_AUTOMAKE=$(which automake)
    fi
    if test -z ${HDF5_AUTOHEADER}; then
        HDF5_AUTOHEADER=$(which autoheader)
    fi
    if test -z ${HDF5_ACLOCAL}; then
        HDF5_ACLOCAL=$(which aclocal)
    fi
    if test -z ${HDF5_LIBTOOL}; then
        case "`uname`" in
        Darwin*)
            # libtool on OS-X is non-gnu
            HDF5_LIBTOOL=$(which glibtool)
            ;;
        *)
            HDF5_LIBTOOL=$(which libtool)
            ;;
        esac
    fi
    if test -z ${HDF5_M4}; then
        HDF5_M4=$(which m4)
    fi
    if test -z ${HDF5_BISON}; then
        HDF5_BISON=$(which bison)
    fi
    if test -z ${HDF5_FLEX}; then
        HDF5_FLEX=$(which flex)
    fi

fi # production


# Make sure that these versions of the autotools are in the path
AUTOCONF_DIR=`dirname ${HDF5_AUTOCONF}`
LIBTOOL_DIR=`dirname ${HDF5_LIBTOOL}`
M4_DIR=`dirname ${HDF5_M4}`
BISON_DIR=`dirname ${HDF5_BISON}`
FLEX_DIR=`dirname ${HDF5_FLEX}`
PATH=${AUTOCONF_DIR}:${LIBTOOL_DIR}:${M4_DIR}:${FLEX_DIR}:${BISON_DIR}:$PATH

# Make libtoolize match the specified libtool
case "`uname`" in
Darwin*)
    # libtoolize on OS-X is non-gnu
    HDF5_LIBTOOLIZE="${LIBTOOL_DIR}/glibtoolize"
    ;;
*)
    HDF5_LIBTOOLIZE="${LIBTOOL_DIR}/libtoolize"
    ;;
esac

# Run autotools in order

# Some versions of libtoolize will suggest that we add ACLOCAL_AMFLAGS
# = '-I m4'. This is already done in commence.am, which is included
# in Makefile.am. You can ignore this suggestion.

echo ${HDF5_LIBTOOLIZE}
if [ "$verbose" = true ] ; then
    ${HDF5_LIBTOOLIZE} --version
fi
${HDF5_LIBTOOLIZE} || exit 1
echo
echo "NOTE: You can ignore the warning about adding -I m4."
echo "      We already do this in an included file."
echo

if test -e "${LIBTOOL_DIR}/../share/aclocal" ; then
    aclocal_include="-I ${LIBTOOL_DIR}/../share/aclocal"
fi
echo ${HDF5_ACLOCAL} ${aclocal_include}
if [ "$verbose" = true ] ; then
    ${HDF5_ACLOCAL} --version
fi
${HDF5_ACLOCAL} ${aclocal_include} || exit 1
echo

echo ${HDF5_AUTOHEADER}
if [ "$verbose" = true ] ; then
    ${HDF5_AUTOHEADER} --version
fi
${HDF5_AUTOHEADER} || exit 1
echo

echo ${HDF5_AUTOMAKE} --add-missing
if [ "$verbose" = true ] ; then
    ${HDF5_AUTOMAKE} --version
fi
${HDF5_AUTOMAKE} --add-missing || exit 1
echo

echo ${HDF5_AUTOCONF}
if [ "$verbose" = true ] ; then
    ${HDF5_AUTOCONF} --version
fi
${HDF5_AUTOCONF} || exit 1
echo

# Run scripts that process source.

# Run trace script
# The trace script adds H5TRACE macros to library source files.  It should
# have no effect on files that don't have HDF5 API macros in them.
echo
echo "Running trace script:"
echo "NOTE: NO TRACE warnings in H5E code are normal and expected."
bin/trace src/H5*.c || exit 1

# Run make_err
# make_err automatically generates the H5E headers that create error message
# types for HDF5.
echo
echo "Running error generation script:"
bin/make_err src/H5err.txt || exit 1

# Run make_vers
# make_vers automatically generates the public headers that define the API version
# macros for HDF5.
echo
echo "Running API version generation script:"
bin/make_vers src/H5vers.txt || exit 1

# Run flex and bison
# automatically generates hl/src/H5LTanalyze.c and hl/src/H5LTparse.c
echo
echo "Running flex/bison:"
cd hl/src
echo "Generate hl/src/H5LTparse.c from hl/src/H5LTparse.y"
if [ "$verbose" = true ] ; then
    ${HDF5_BISON} --version
fi
${HDF5_BISON} -pH5LTyy -o H5LTparse.c -d H5LTparse.y

echo "Generate hl/src/H5LTanalyze.c from hl/src/H5LTanalyze.l"
if [ "$verbose" = true ] ; then
    ${HDF5_FLEX} --version
fi
${HDF5_FLEX} --nounistd -PH5LTyy -o H5LTanalyze.c H5LTanalyze.l

# fix H5LTparse.c to declare H5LTyyparse return type as an hid_t
# instead of int.  Currently the generated function H5LTyyparse is
# generated with a return value of type int, which is a mapping to the
# flex yyparse function.  The return value in the HL library should be
# an hid_t. 
# I propose to not use flex to generate this function, but for now I am 
# adding a perl command to find and replace this function declaration in
# H5LTparse.c.
perl -0777 -pi -e 's/int\nyyparse/hid_t\nyyparse/igs' H5LTparse.c
perl -0777 -pi -e 's/int H5LTyyparse/hid_t H5LTyyparse/igs' H5LTparse.c
cd ../..

echo
exit 0

