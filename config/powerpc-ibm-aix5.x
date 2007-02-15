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

# Configuration file for building on the IBM POWER AIX platforms.
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
    # Use stdc99 to use C99 standard.
    CFLAGS="-qlanglvl=stdc99 -D_LARGE_FILES $CFLAGS"
    DEBUG_CFLAGS="-g -qfullpath"
    DEBUG_CPPFLAGS=
    # -O causes test/dtypes to fail badly. Turn it off for now.
    PROD_CFLAGS=""
    PROD_CPPFLAGS=
    PROFILE_CFLAGS="-g -qfullpath -pg"
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

#----------------------------------------------------------------------------
# Values for overriding configuration tests when cross compiling.
# This includes compiling on some machines where the serial front end
# compiles for a parallel back end.

# Set this to `yes' or `no' depending on whether the target is big
# endian or little endian.
hdf5_cv_printf_ll=${hdf5_cv_printf_ll='ll'}
ac_cv_c_bigendian=${ac_cv_c_bigendian='yes'}
ac_cv_header_stdc=${ac_cv_header_stdc='yes'}
ac_cv_header_sys_ioctl_h=${ac_cv_header_sys_ioctl_h=yes}

# cache the sizeof of "standard C types" so that configure can run faster.
ac_cv_sizeof_char=${ac_cv_sizeof_char=1}
ac_cv_sizeof_short=${ac_cv_sizeof_short=2}
ac_cv_sizeof_int=${ac_cv_sizeof_int=4}
ac_cv_sizeof_long_long=${ac_cv_sizeof_long_long=8}
ac_cv_sizeof___int64=${ac_cv_sizeof___int64=8}
ac_cv_sizeof_float=${ac_cv_sizeof_float=4}
ac_cv_sizeof_double=${ac_cv_sizeof_double=8}
ac_cv_sizeof_long_double=${ac_cv_sizeof_long_double=8}
ac_cv_sizeof_int8_t=${ac_cv_sizeof_int8_t=1}
ac_cv_sizeof_uint8_t=${ac_cv_sizeof_uint8_t=1}
ac_cv_sizeof_int_least8_t=${ac_cv_sizeof_int_least8_t=1}
ac_cv_sizeof_uint_least8_t=${ac_cv_sizeof_uint_least8_t=1}
# Do not cache int_fast8_t since the vendor changes often.
ac_cv_sizeof_int16_t=${ac_cv_sizeof_int16_t=2}
ac_cv_sizeof_uint16_t=${ac_cv_sizeof_uint16_t=2}
ac_cv_sizeof_int_least16_t=${ac_cv_sizeof_int_least16_t=2}
ac_cv_sizeof_uint_least16_t=${ac_cv_sizeof_uint_least16_t=2}
# Do not cache int_fast16_t since the vendor changes often.
ac_cv_sizeof_int32_t=${ac_cv_sizeof_int32_t=4}
ac_cv_sizeof_uint32_t=${ac_cv_sizeof_uint32_t=4}
ac_cv_sizeof_int_least32_t=${ac_cv_sizeof_int_least32_t=4}
ac_cv_sizeof_uint_least32_t=${ac_cv_sizeof_uint_least32_t=4}
ac_cv_sizeof_int_fast32_t=${ac_cv_sizeof_int_fast32_t=4}
ac_cv_sizeof_uint_fast32_t=${ac_cv_sizeof_uint_fast32_t=4}
ac_cv_sizeof_int64_t=${ac_cv_sizeof_int64_t=8}
ac_cv_sizeof_uint64_t=${ac_cv_sizeof_uint64_t=8}
ac_cv_sizeof_int_least64_t=${ac_cv_sizeof_int_least64_t=8}
ac_cv_sizeof_uint_least64_t=${ac_cv_sizeof_uint_least64_t=8}
ac_cv_sizeof_int_fast64_t=${ac_cv_sizeof_int_fast64_t=8}
ac_cv_sizeof_uint_fast64_t=${ac_cv_sizeof_uint_fast64_t=8}

# Don't cache long since it varies between 32 and 64 bits
#ac_cv_sizeof_long=${ac_cv_sizeof_long=4}

# Don't cache size_t and off_t because they depend on if -D_LARGE_FILES is used
#ac_cv_sizeof_size_t=${ac_cv_sizeof_size_t=4}
#ac_cv_sizeof_off_t=${ac_cv_sizeof_off_t=8}

# With poe version 3.2.0.19 or lower(using lpp -l all | grep ppe.poe to check the version number, 
# IBM MPI-IO implementation has a bug, 
#it cannot generate correct MPI derived datatype. Please uncomment the following line:
#hdf5_mpi_complex_derived_datatype_works=${hdf5_mpi_complex_derived_datatype_works='no'}
