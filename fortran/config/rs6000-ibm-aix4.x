#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details.

# Cross compiling defaults
ac_cv_c_bigendian=${ac_cv_c_bigendian='yes'}
hdf5_cv_printf_ll=${hdf5_cv_printf_ll='ll'}


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
    F9X=xlf
fi

if test "X-" = "X-$f9x_flags_set"; then
    F9XSUFFIXFLAG="-qsuffix=f=f90"
    FFLAGS="$FFLAGS -static -O ${F9XSUFFIXFLAG} -qmoddir=./ -k"
    FSEARCH_DIRS="-I./ -I../src"
    DEBUG_FFLAGS="-O"
    PROD_FFLAGS="-O"
    PROFILE_FFLAGS="-O"
    f9x_flags_set=yes
fi
