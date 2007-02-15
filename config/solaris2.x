#							-*- shell-script -*-
#
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


# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `sunpro cc'
if test "X-" =  "X-$CC"; then
  CC=cc
  CC_BASENAME=cc
fi

# Try gcc compiler flags
. $srcdir/config/gnu-flags

# Try solaris native compiler flags
if test "X-" = "X-$cc_flags_set"; then
  CFLAGS="$CFLAGS -erroff=%none -DBSD_COMP"
  DEBUG_CFLAGS="-g -xildoff"
  DEBUG_CPPFLAGS=
  PROD_CFLAGS="-O -s"
  PROD_CPPFLAGS=
  PROFILE_CFLAGS=-xpg
  PROFILE_CPPFLAGS=
  cc_flags_set=yes

  # Turn off optimization flag for SUNpro compiler versions 4.x which
  # have an optimization bug.  Version 5.0 works.
  ($CC -V 2>&1) | grep -s 'cc: .* C 4\.' >/dev/null 2>&1 \
       && PROD_CFLAGS="`echo $PROD_CFLAGS | sed -e 's/-O//'`"
fi

# Add socket lib for the Stream Virtual File Driver
LIBS="$LIBS -lsocket"
