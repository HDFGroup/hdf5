#							-*- shell-script -*-
#
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

# Hard set flag to indicate that the 'unsigned long long' to floating-point
# value conversion are broken by the compilers (as of 4/27/04 - QAK)
hdf5_cv_sw_ulong_to_fp_bottom_bit_works=${hdf5_cv_sw_ulong_to_fp_bottom_bit_works='no'}

# Set flags to avoid conversion between 'long double' and integers because of 
# SGI's compiler problems.  For both IRIX64 6.5 and IRIX 6.5, the compilers
# have the following problems,
#       long double -> signed char : incorrect rounding
#       long double -> unsigned char : incorrect rounding
#       long double -> short : incorrect rounding
#       long double -> unsigned short : incorrect rounding
#       long double -> long or long long: incorrect value
#       long double -> unsigned long or long long : incorrect value
#
#       long or long long -> long double : correct value but incorrect bit pattern
#       unsigned long or long long -> long double : correct value but incorrect bit pattern
# (1/5/05 - SLU)
hdf5_cv_sw_ldouble_to_int_works=${hdf5_cv_sw_ldouble_to_int_works='no'}
hdf5_cv_sw_integer_to_ldouble_works=${hdf5_cv_sw_integer_to_ldouble_works='no'}
