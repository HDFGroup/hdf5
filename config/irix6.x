#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details

# The default compiler is `cc' and there is no ranlib.
if test "X-" =  "X-$CC"; then
    CC='cc -n32'
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
        CFLAGS="$CFLAGS -ansi"

	# Always turn off these compiler warnings:
	#    1174:  function declared but not used
	#    1429:  the `long long' type is not standard
	#    1209:  constant expressions
	#    1196:  __vfork() (this is an SGI config problem)
        CFLAGS="$CFLAGS -woff 1174,1429,1209,1196"

	# Always turn off these loader warnings:
	#      47:  linked module might degrade performance
	#      84:  a library is not used
	#      85:  duplicate definition preemption
	#     134:  duplicate weak definition preemption
	CFLAGS="$CFLAGS -Wl,-woff,47,-woff,84,-woff,85,-woff,134"

	# Extra debugging flags
	DEBUG_CFLAGS=-g
	DEBUG_CPPFLAGS=

	# Extra production flags
	# Note: higher optimizations relax alignment requirements needed.
	PROD_CFLAGS=-O1
	PROD_CPPFLAGS=

	# Extra profiling flags
	PROFILE_CFLAGS=-pg
	PROFILE_CPPFLAGS=
	;;
esac
