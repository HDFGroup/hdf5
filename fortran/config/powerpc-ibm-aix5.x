#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details.

# Use AIX supplied C compiler by default, xlc for serial, mpcc_r for parallel.
# Use -D_LARGE_FILES by default to support large file size.
if test "X-" =  "X-$CC"; then
  if test "X-$enable_parallel" = "X-yes"; then
    CC='mpcc_r -D_LARGE_FILES'
    CC_BASENAME=mpcc_r
    RUNPARALLEL=${RUNPARALLEL="MP_PROCS=3 MP_TASKS_PER_NODE=3 poe"}
  else
    CC='xlc -D_LARGE_FILES'
    CC_BASENAME=xlc
  fi
fi

#----------------------------------------------------------------------------
# Compiler flags. The CPPFLAGS values should not include package debug
# flags like `-DH5G_DEBUG' since these are added with the
# `--enable-debug' switch of configure.

case $CC_BASENAME in
    xlc|mpcc_r)
	# Turn off shared lib option.  It causes some test suite to fail.
	enable_shared="${enable_shared:-no}"
	# CFLAGS must be set else configure set it to -g
	CFLAGS="$CFLAGS"
	DEBUG_CFLAGS="-g"
	DEBUG_CPPFLAGS=
	# -O causes test/dtypes to fail badly. Turn it off for now.
	PROD_CFLAGS=""
	PROD_CPPFLAGS=
	PROFILE_CFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;

    gcc)
	. $srcdir/config/gnu-flags
	;;

    *)
	CFLAGS="$CFLAGS -ansi"
	DEBUG_CFLAGS="-g"
	DEBUG_CPPFLAGS=
	PROD_CFLAGS="-O"
	PROD_CPPFLAGS=
	PROFILE_CFLAGS="-pg"
	PROFILE_CPPFLAGS=
	;;
esac

# Cross compiling defaults
ac_cv_c_bigendian=${ac_cv_c_bigendian='yes'}
ac_cv_header_stdc=${ac_cv_header_stdc='yes'}
ac_cv_sizeof_short=${ac_cv_sizeof_short=2}
ac_cv_sizeof_int=${ac_cv_sizeof_int=4}
ac_cv_sizeof_long=${ac_cv_sizeof_long=4}
ac_cv_sizeof_long_long=${ac_cv_sizeof_long_long=8}
ac_cv_sizeof_float=${ac_cv_sizeof_float=4}
ac_cv_sizeof_double=${ac_cv_sizeof_double=8}
ac_cv_sizeof_long_double=${ac_cv_sizeof_long_double=8}
ac_cv_sizeof_int=${ac_cv_sizeof_int=4}
# Don't cache size_t and off_t because they depend on if -D_LARGE_FILES is used
#ac_cv_sizeof_size_t=${ac_cv_sizeof_size_t=4}
#ac_cv_sizeof_off_t=${ac_cv_sizeof_off_t=8}


# The default Fortran 90 compiler

#
# HDF5 integers
#
# 	R_LARGE is the number of digits for the bigest integer supported.
#	R_INTEGER is the number of digits in INTEGER
#
# (for the AIX architechture)
#
R_LARGE=18
R_INTEGER=9
HSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HSSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HID_T='SELECTED_INT_KIND(R_INTEGER)'
SIZE_T='SELECTED_INT_KIND(R_INTEGER)'
OBJECT_NAMELEN_DEFAULT_F=-1

if test "X-" = "X-$F9X"; then
  if test "X-$enable_parallel" = "X-yes"; then
    F9X=mpxlf_r
  else
    F9X=xlf
  fi
fi

if test "X-" = "X-$f9x_flags_set"; then
    F9XSUFFIXFLAG="-qsuffix=f=f90"
    FFLAGS="-static -O ${F9XSUFFIXFLAG} -qmoddir=./ -k"
    FSEARCH_DIRS="-I./ -I../src"
    DEBUG_FFLAGS="-O"
    PROD_FFLAGS="-O"
    PROFILE_FFLAGS="-O"
    f9x_flags_set=yes
fi
