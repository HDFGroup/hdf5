#							-*- shell-script -*-
#
# This file is part of the HDF5 build script.  It is processed shortly
# after configure starts and defines, among other things, flags for
# the various compile modes.
#
# See BlankForm in this directory for details.

# CFLAGS must be set else configure set it to -g
CFLAGS="$CFLAGS"

# Cross compiling defaults
ac_cv_c_bigendian=${ac_cv_c_bigendian='yes'}
