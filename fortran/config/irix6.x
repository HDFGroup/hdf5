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
# See BlankForm in this directory for details.

# Use SGI supplied C compiler by default.  There is no ranlib
if test "X-" =  "X-$CC"; then
    CC='cc'
    CC_BASENAME=cc
fi
RANLIB=:

# Compiler flags
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
        # Check for old versions of the compiler that don't work right.
        case "`$CC -version 2>&1 |head -1`" in
	    "Mongoose Compilers: Version 7.00")
		echo "  +---------------------------------------------------+"
		echo "  | You have an old version of cc (Mongoose Compilers |"
		echo "  | version 7.00).  Please upgrade to MIPSpro version |"
		echo "  | 7.2.1.2m (patches are available from the SGI web  |"
		echo "  | site).  The 7.00 version may generate incorrect   |"
		echo "  | code, especially when optimizations are enabled.  |"
		echo "  +---------------------------------------------------+"
		sleep 5
		;;
	esac

	# Do *not* use -ansi because it prevents hdf5 from being able
	# to read modification dates from the file. On some systems it
	# can also result in compile errors in system header files
	# since hdf5 includes a couple non-ANSI header files.
	#CFLAGS="$CFLAGS -ansi"

	# Always turn off these compiler warnings for the -64 compiler:
	#    1174:  function declared but not used
	#    1196:  __vfork() (this is an SGI config problem)
	#    1209:  constant expressions
	#    1429:  the `long long' type is not standard
	#    1685:  turn off warnings about turning off invalid warnings
	#    3201:  remark - parameter not referenced
	CFLAGS="$CFLAGS -woff 1174,1429,1209,1196,1685,3201"

	# Always turn off these compiler warnings for the old compiler:
	#    799:   the `long long' type is not standard
	#    803:   turn off warnings about turning off invalid warnings
	#    835:   __vfork() (this is an SGI config problem)
	CFLAGS="$CFLAGS -woff 799,803,835"

	# Always turn off these loader warnings:
	# (notice the peculiar syntax)
	#      47:  branch instructions that degrade performance on R4000
	#      84:  a library is not used
	#      85:  duplicate definition preemption (from -lnsl)
	#     134:  duplicate weak definition preemption (from -lnsl)
	CFLAGS="$CFLAGS -Wl,-woff,47,-woff,84,-woff,85,-woff,134"

	# Extra debugging flags
	DEBUG_CFLAGS="-g -fullwarn"
	DEBUG_CPPFLAGS=

	# Extra production flags
	PROD_CFLAGS="-64 -mips4 -O -s"
	PROD_CPPFLAGS=

	# Extra profiling flags
	PROFILE_CFLAGS=-pg
	PROFILE_CPPFLAGS=
	;;
esac

# Use SGI supplied C compiler by default.  There is no ranlib
if test "X-" =  "X-$F9X"; then
    F9X="f90"
    F9XSUFFIXFLAG=""
    FSEARCH_DIRS=""
    FFLAGS="$FFLAGS -64 -mips4 -O -s"
    DEBUG_FFLAGS="-64 -mips4 -O -s"
    PROD_FFLAGS="-64 -mips4 -O -s"
    PROFILE_FFLAGS="-64 -mips4 -O -s"
fi

# The default Fortran 90 compiler

#
# HDF5 integers
#
# 	R_LARGE is the number of digits for the bigest integer supported.
#	R_INTEGER is the number of digits in INTEGER
#
# (for the IRIX architechture)
#
R_LARGE=18
R_INTEGER=9
HADDR_T='SELECTED_INT_KIND(R_LARGE)'
HSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HSSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HID_T='SELECTED_INT_KIND(R_INTEGER)'
SIZE_T='SELECTED_INT_KIND(R_LARGE)'
OBJECT_NAMELEN_DEFAULT_F=-1

if test "X-" = "X-$F9X"; then
    F9X=f90
fi

if test "X-" = "X-$f9x_flags_set"; then
    F9XSUFFIXFLAG=""
    FSEARCH_DIRS=""
    FFLAGS="$FFLAGS -64 -mips4 -O -s"
    DEBUG_FFLAGS="-64 -mips4 -O -s"
    PROD_FFLAGS="-64 -mips4 -O -s"
    PROFILE_FFLAGS="-64 -mips4 -O -s"
    f9x_flags_set=yes
fi
