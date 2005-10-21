#							-*- shell-script -*-
#
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and 
# is linked from the top-level documents page.  It can also be found at
# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.


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
