#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `gcc'
if test "X-" =  "X-$CC"; then
    CC=gcc
    CC_BASENAME=gcc
fi

# Try gcc compiler flags
. $srcdir/config/gnu-flags

# Try solaris native compiler flags
if test "X-" = "X-$cc_flags_set"; then
    CFLAGS="-erroff=%none"
    DEBUG_CFLAGS=-g
    DEBUG_CPPFLAGS="-DH5F_LOW_DFLT=H5F_LOW_SEC2"
    PROD_CFLAGS=-xO2
    PROD_CPPFLAGS=
    PROFILE_CFLAGS=-xpg
    PROFILE_CPPFLAGS=
    cc_flags_set=yes
fi
