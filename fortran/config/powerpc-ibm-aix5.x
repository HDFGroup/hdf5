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

# Use AIX supplied C compiler by default, xlc for serial, mpcc_r for parallel.
# Use -D_LARGE_FILES by default to support large file size.
if test "X-" =  "X-$CC"; then
  if test "X-$enable_parallel" = "X-yes"; then
    CC=mpcc_r
    CC_BASENAME=mpcc_r
  else
    CC=xlc
    CC_BASENAME=xlc
  fi
fi

# Define RUNPARALLEL if parallel mode is enabled or a parallel compiler used.
if test "X-$enable_parallel" = "X-yes" -o X-$CC_BASENAME = X-mpcc_r; then
    RUNPARALLEL=${RUNPARALLEL="env MP_PROCS=\$\${NPROCS:=3} MP_TASKS_PER_NODE=\$\${NPROCS:=3} poe"}
fi


#----------------------------------------------------------------------------
# Compiler flags. The CPPFLAGS values should not include package debug
# flags like `-DH5G_DEBUG' since these are added with the
# `--enable-debug' switch of configure.

case $CC_BASENAME in
    xlc|mpcc_r)
	# Turn off shared lib option.  It causes some test suite to fail.
	enable_shared="${enable_shared:-no}"
        # Use -D_LARGE_FILES by default to support large file size.
        CFLAGS="-qlanglvl=stdc99 -D_LARGE_FILES $CFLAGS"
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
HADDR_T='SELECTED_INT_KIND(R_LARGE)'
HSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HSSIZE_T='SELECTED_INT_KIND(R_LARGE)'
HID_T='SELECTED_INT_KIND(R_INTEGER)'
SIZE_T='SELECTED_INT_KIND(R_INTEGER)'
OBJECT_NAMELEN_DEFAULT_F=-1

if test "X-" = "X-$F9X"; then
  if test "X-$enable_parallel" = "X-yes"; then
    F9X=mpxlf90_r
  else
    F9X=xlf90
  fi
fi

if test "X-" = "X-$f9x_flags_set"; then
    F9XSUFFIXFLAG="-qsuffix=f=f90"
    FFLAGS="$FFLAGS -O ${F9XSUFFIXFLAG}"
    FSEARCH_DIRS="-I./ -I../src"
    DEBUG_FFLAGS="-O"
    PROD_FFLAGS="-O"
    PROFILE_FFLAGS="-O"
    f9x_flags_set=yes
fi
