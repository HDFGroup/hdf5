#							-*- shell-script -*-

# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.

#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `sunpro cc'
if test -z "$CXX"; then
    CXX=CC
    CXX_BASENAME=CC
fi

# Try gcc compiler flags
#. $srcdir/config/gnu-flags

cxx_version="`$CXX -V 2>&1 |grep 'WorkShop' |\
                sed 's/.*WorkShop.*C++ \([0-9\.]*\).*/\1/'`"

cxx_vers_major=`echo $cxx_version | cut -f1 -d.`
cxx_vers_minor=`echo $cxx_version | cut -f2 -d.`
cxx_vers_patch=`echo $cxx_version | cut -f3 -d.`

# Specify the "-features=tmplife" if the compiler can handle this...
if test -n "$cxx_version"; then
  if test $cxx_vers_major -ge 5 -a $cxx_vers_minor -ge 3 -o $cxx_vers_major -gt 5; then
    CXXFLAGS="$CXXFLAGS -features=tmplife"
  fi
fi

# Try solaris native compiler flags
if test -z "$cxx_flags_set"; then
    CXXFLAGS="$CXXFLAGS -instances=static"
    CPPFLAGS="$CPPFLAGS -LANG:std"
    LIBS="$LIBS -lsocket"
    DEBUG_CXXFLAGS=-g
    DEBUG_CPPFLAGS=
    PROD_CXXFLAGS="-O -s"
    PROD_CPPFLAGS=
    PROFILE_CXXFLAGS=-xpg
    PROFILE_CPPFLAGS=
    cxx_flags_set=yes
fi
