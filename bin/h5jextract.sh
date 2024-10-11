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
## Update HDF5 java/jsrc after the HDF5 software has been built  ##
## For help page, use "h5jextract -help"                         ##

# show help page
usage() {
  # A wonderfully informative "usage" message.
  echo "usage: $prog_name [OPTIONS]"
  echo "  OPTIONS:"
  echo "    -help|help        This help message"
  echo "    -echo             Show all the shell commands executed"
  echo "    -jextract=DIR     Directory to find the jextract binary"
  echo "                          [default: current <user home>/jextract/bin]"
  echo "    -sourcedir=DIR    Directory for the HDF5 source files"
  echo "                          [default: current directory]"
  echo "    -includedir=DIR   Directory for the HDF5 header files"
  echo "                        [default: current directory/include]"
  echo "                        [should be set to the public only headers]"
  echo " "
  exit $EXIT_FAILURE
}

# Report Error message
ERROR()
{
    echo "***ERROR***"
    echo "$1"
}

# Main
#
############################################################################
# Initialization
prefix=
jextract=
includedir=

# Parse options
for arg in $@ ; do
  case "$arg" in
    -jextract=*)
      jextract="`echo $arg | cut -f2 -d=`"
      ;;
    -sourcedir=*)
      sourcedir="`echo $arg | cut -f2 -d=`"
      ;;
    -includedir=*)
      includedir="`echo $arg | cut -f2 -d=`"
      ;;
    -echo)
      set -x
      ;;
    -help|help)
      usage
      ;;
    *)
      ERROR "Unknown Option($arg)"
      usage
      exit $EXIT_FAILURE
      ;;
  esac
done

# Set to default value, one above where i am, if not given by user
if [ -z "$sourcedir" ]; then
    sourcedir=`(cd ..;pwd)`
fi
if [ -z "$includedir" ]; then
    includedir='${sourcedir}'/include # use single quotes to prevent expansion of $
fi
if [ -z "$jextract" ]; then
    jextract='${HOME}'/jextract/bin     # use single quotes to prevent expansion of $
fi


$jextract/jextract \
  --include-dir $includedir \
  --output $sourcedir/java/jsrc \
  --target-package org.hdfgroup.javahdf5 \
  --library hdf5 \
  $includedir/hdf5.h
