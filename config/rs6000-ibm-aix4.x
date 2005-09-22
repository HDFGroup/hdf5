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

if test "X-" = "X-$FC"; then
  FC=xlf
fi

if test "X-" = "X-$f9x_flags_set"; then
  F9XSUFFIXFLAG="-qsuffix=f=f90"
  FCFLAGS="$FCFLAGS -static -O ${F9XSUFFIXFLAG} -qmoddir=./ -k"
  FSEARCH_DIRS="-I./ -I../src"
  DEBUG_FCFLAGS="-O"
  PROD_FCFLAGS="-O"
  PROFILE_FCFLAGS="-O"
  f9x_flags_set=yes
fi
# IBM MPI-IO doesn't handle complicated derived data type correctly.
hdf5_mpi_complex_derived_datatype_works=${hdf5_mpi_complex_derived_datatype_works='no'}
