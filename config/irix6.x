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
if test "X-" = "X-$CC"; then
    CC='cc'
    CC_BASENAME=cc
    # use c99 compiler if available.
    if `c99 -version >/dev/null 2>&1` ; then
        CC='c99'
    fi
fi
RANLIB=:

# Compiler flags
case "X-$CC_BASENAME" in
  X-gcc)
    . $srcdir/config/gnu-flags
    ;;

  *)
    if [ "$CC_BASENAME" = "cc" ] || ($CC -version 2>&1 | grep -s "MIPSpro Compilers") 2>&1 > /dev/null; then
      # use these flags if this is the SGI cc compiler or some compiler
      # command that eventually uses the SGI cc compiler.

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

      # Always turn off these compiler warnings for the -64 compiler:
      #    1174:  function declared but not used
      #    1196:  __vfork() (this is an SGI config problem)
      #    1209:  constant expressions
      #    1429:  the `long long' type is not standard
      #    1685:  turn off warnings about turning off invalid warnings
      #    3201:  remark - parameter not referenced
      #CFLAGS="$CFLAGS -woff 1174,1429,1209,1196,1685,3201"
      CFLAGS="$CFLAGS -woff 1209,3201"

      # Always turn off these compiler warnings for the old compiler:
      #    799:   the `long long' type is not standard
      #    803:   turn off warnings about turning off invalid warnings
      #    835:   __vfork() (this is an SGI config problem)
      #CFLAGS="$CFLAGS -woff 799,803,835"

      # Always turn off these loader warnings:
      # (notice the peculiar syntax)
      #      47:  branch instructions that degrade performance on R4000
      #      84:  a library is not used
      #      85:  duplicate definition preemption (from -lnsl)
      #     134:  duplicate weak definition preemption (from -lnsl)
      CFLAGS="$CFLAGS -Wl,-woff,47,-woff,84,-woff,85,-woff,134"
    fi

    # Extra debugging flags
    DEBUG_CFLAGS="-g -fullwarn"
    DEBUG_CPPFLAGS=

    # Extra production flags
    PROD_CFLAGS="-O -OPT:Olimit=0 -s"
    PROD_CPPFLAGS=

    # Extra profiling flags
    PROFILE_CFLAGS=
    PROFILE_CPPFLAGS=
    ;;
esac

# For IRIX 6.5, any version that is older than MIPSpro 7.3.1.3m, 
# the MPI derived datatype is not working.
# Versions 7.4.2m or newer work.
# Fix $hdf5_mpi_complex_derived_datatype_works if it is not set and is using cc.
if [ -z "$hdf5_mpi_complex_derived_datatype_works" -a $CC_BASENAME = cc ]; then
    ccversion=`$CC -version 2>&1 | sed -e 's/.*Version //p'`
    ccversion1=`echo $ccversion | cut -f1 -d.`
    ccversion2=`echo $ccversion | cut -f2 -d.`
    # Assume all versions 7.4.* or newer are okay
    # and assume ccversion2 is never larger than 99.
    ccversionval=`expr $ccversion1 \* 100 + $ccversion2`
    if [ $ccversionval -lt 704 ]; then
        hdf5_mpi_complex_derived_datatype_works='no'
    fi
fi

