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
#
# Note that aclocal will attempt to include libtool's share/aclocal
# directory.
#
# This script takes two options:
#
# -p/--production
#
# When this is selected, the autotools versions are set to the paths
# and versions used by The HDF Group to produce the released versions
# of the library. # -s/--process_source
#
# When this is selected, several scripts are run that process the HDF5
# source code to add things like trace and error macros. It is not
# necessary to run these if you have just updated configure.ac or
# Makefile.am files.

# Defaults are not production, don't run source processing.
production=false
process_source=false

optspec=":hps-"
while getopts "$optspec" optchar; do
    case "${optchar}" in
    -)
        case "${OPTARG}" in
            production)
                echo "Setting production mode..."
                production=true
                ;;
            process_source)
                process_source=true
                ;;
            *)
                if [ "$OPTERR" = 1 ] && [ "${optspec:0:1}" != ":" ]; then
                    echo "Unknown option --${OPTARG}" >&2
                fi
                ;;
        esac;;
    h)
        echo "usage: $0 [-s|--process_source] [-p|--production]"
        echo
        echo "      -p      Used by THG to ensure that particular versions"
        echo "              of the autotools are used and hard-codes"
        echo "              autotools paths to THG machines. Not for"
        echo "              non-HDF-Group users!"
        echo
        echo "      -s      Run source/API processsing scripts."
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
    s)
        process_source=true
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
    # Hard-code canonical HDF Group autotools locations and ensure
    # version numbers are correct.

    # Production versions of the autotools
    AUTOCONF_VERSION="autoconf (GNU Autoconf) 2.69"
    AUTOMAKE_VERSION="automake (GNU automake) 1.14.1"
    AUTOHEADER_VERSION="autoheader (GNU Autoconf) 2.69"
    ACLOCAL_VERSION="aclocal (GNU automake) 1.14.1"
    LIBTOOL_VERSION="(GNU libtool) 2.4.2"
    M4_VERSION="m4 (GNU M4) 1.4.17"

    # If paths to autotools are not specified, assume tools are
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

else

    # Not in production mode
    #
    # If paths to autotools are not specified, use whatever the system
    # has installed as the default.
    if test -z ${HDF5_AUTOCONF}; then
        HDF5_AUTOCONF=autoconf
    fi
    if test -z ${HDF5_AUTOMAKE}; then
        HDF5_AUTOMAKE=automake
    fi
    if test -z ${HDF5_AUTOHEADER}; then
        HDF5_AUTOHEADER=autoheader
    fi
    if test -z ${HDF5_ACLOCAL}; then
        HDF5_ACLOCAL=aclocal
    fi
    if test -z ${HDF5_LIBTOOL}; then
        HDF5_LIBTOOL=libtool
    fi
    if test -z ${HDF5_M4}; then
        HDF5_M4=m4
    fi

fi # production

# Make sure that these versions of the autotools are in the path
AUTOCONF_DIR=`dirname ${HDF5_AUTOCONF}`
LIBTOOL_DIR=`dirname ${HDF5_LIBTOOL}`
M4_DIR=`dirname ${HDF5_M4}`
PATH=${AUTOCONF_DIR}:${M4_DIR}:$PATH

# Run autotools in order
if test -e "${LIBTOOL_DIR}/../share/aclocal" ; then
    aclocal_include="-I ${LIBTOOL_DIR}/../share/aclocal"
fi
echo ${HDF5_ACLOCAL} ${aclocal_include}
${HDF5_ACLOCAL} ${aclocal_include} || exit 1

echo ${HDF5_AUTOHEADER}
${HDF5_AUTOHEADER} || exit 1

echo ${HDF5_AUTOMAKE} --add-missing
${HDF5_AUTOMAKE} --add-missing || exit 1

echo ${HDF5_AUTOCONF}
${HDF5_AUTOCONF} || exit 1

# If source processing was eanbled using -s/--process_source, run the
# source processing scripts.
if [ "$process_source" = true ] ; then

    # Run trace script
    # The trace script adds H5TRACE macros to library source files.  It should
    # have no effect on files that don't have HDF5 API macros in them.
    echo
    echo "    Running trace script:"
    bin/trace src/H5*.c || exit 1

    # Run make_err
    # make_err automatically generates the H5E headers that create error message
    # types for HDF5.
    echo
    echo "    Running error generation script:"
    bin/make_err src/H5err.txt || exit 1

    # Run make_vers
    # make_vers automatically generates the public headers that define the API version
    # macros for HDF5.
    echo
    echo "    Running API version generation script:"
    bin/make_vers src/H5vers.txt || exit 1

    # Run flex
    # automatically generates the lexical file for hl/src/H5LTanalyze.c
    echo
    echo "    Running flex generation script:"
    bin/genltanalyze || exit 1

fi # process_source

exit 0

