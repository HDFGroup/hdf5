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
	PROD_CFLAGS=-O
	PROD_CPPFLAGS=

	# Extra profiling flags
	PROFILE_CFLAGS=-pg
	PROFILE_CPPFLAGS=
	;;
esac
