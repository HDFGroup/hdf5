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

# The default compiler is `cc' and there is no ranlib.
if test "X-" = "X-$CC"; then
  CC=cc
  CC_BASENAME=cc
fi
RANLIB=:

case "X-$CC_BASENAME" in
  X-gcc)
    CFLAGS="$CFLAGS -Wsign-compare" #Only works for some versions
    DEBUG_CFLAGS="-g -fverbose-asm"
    DEBUG_CPPFLAGS=
    PROD_CFLAGS="-O3"
	PROD_CPPFLAGS=
    PROFILE_CFLAGS="-pg"
    PROFILE_CPPFLAGS=
    ;;

  *)
    # Do *not* use -ansi because it prevents hdf5 from being able
    # to read modification dates from the file. On some systems it
    # can also result in compile errors in system header files
    # since hdf5 includes a couple non-ANSI header files.
    #CFLAGS="$CFLAGS -ansi"

    # Always turn off these compiler warnings:
    CFLAGS="$CFLAGS -woff 799"

    # Extra debugging flags
    DEBUG_CFLAGS=-g
    DEBUG_CPPFLAGS=

    # Extra production flags
    # Note: higher optimizations relax alignment requirements needed.
    PROD_CFLAGS="-O -s"
    PROD_CPPFLAGS=

    # Extra profiling flags
    PROFILE_CFLAGS=-pg
    PROFILE_CPPFLAGS=
    ;;
esac
