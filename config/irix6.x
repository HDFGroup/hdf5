#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `cc' and there is no ranlib.
if test "X-" =  "X-$CC"; then
    CC=cc
    CC_BASENAME=cc
fi
RANLIB=:

case "X-$CC_BASENAME" in
    X-gcc)
	CFLAGS="$CFLAGS -Wsign-compare" #Only works for some versions
	DEBUG_CFLAGS="-g -fverbose-asm"
	DEBUG_CPPFLAGS="-DH5F_OPT_SEEK=0 -DH5F_LOW_DFLT=H5F_LOW_SEC2"
	PROD_CFLAGS="-O3"
	PROD_CPPFLAGS=
	PROFILE_CFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;

    *)
	# Always turn off these compiler warnings:
	#    1174:  function declared but not used
	#    1429:  the `long long' type is not standard
	#    1209:  constant expressions
	#    1196:  __vfork() (this is an SGI config problem)
	# Always turn off these loader warnings:
	#      84:  a library is not used
	CFLAGS="$CFLAGS -ansi -n32 -woff 1174,1429,1209,1196 -Wl,-woff,84"
	DEBUG_CFLAGS=-g
	DEBUG_CPPFLAGS=
	PROD_CFLAGS=-O
	PROD_CPPFLAGS=
	PROFILE_CFLAGS=-pg
	PROFILE_CPPFLAGS=
	;;
esac
