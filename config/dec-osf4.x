#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for detailed information.



# The default compiler is `cc'
if test "X-" =  "X-$CC"; then
    CC=cc
    CC_BASENAME=cc
fi

# Compiler flags
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
        # Watch out for some versions of the compiler
        case "`($CC -V |head -1) 2>/dev/null`" in
	    "DEC C V5.2-038 "*)
		echo "  +------------------------------------------------+"
		echo "  | You have an old version of cc. Please upgrade. |"
		echo "  | Continuing anyway, but code generation might be|"
		echo "  | incorrect, especially if optimizations are     |"
		echo "  | enabled (look for -O)                          |"
		echo "  +------------------------------------------------+"
		sleep 5
		;;
	esac

	# Debugging
	DEBUG_CFLAGS="-g -std -verbose -warnprotos"
	DEBUG_CPPFLAGS="-DH5F_OPT_SEEK=0 -DH5F_LOW_DFLT=H5F_LOW_SEC2"

	# Production
	PROD_CFLAGS="-g0 -verbose -warnprotos -std -O4 -arch host -tune host -ansi_args -fp_reorder -readonly_strings -inline speed"
	PROD_CPPFLAGS="-D_INTRINSICS -D_INLINE_INTRINSICS"

	# Profiling
	PROFILE_CFLAGS="-pg -std -verbose -warnprotos"
	PROFILE_CPPFLAGS=
	;;
esac
