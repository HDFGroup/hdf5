#							-*- shell-script -*-
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

# Try solaris native compiler flags
if test -z "$cxx_flags_set"; then
    CFLAGS="-erroff=%none"
    LIBS="$LIBS -lsocket"
    DEBUG_CFLAGS=-g
    DEBUG_CPPFLAGS=
    PROD_CFLAGS="-O -s"
    PROD_CPPFLAGS=
    PROFILE_CFLAGS=-xpg
    PROFILE_CPPFLAGS=
    cxx_flags_set=yes
fi
