#! /bin/sh
# A simple script to reconfigure autotools for HDF5.
# The autotools include a reconfigure script, but this script
# has the paths of autoconf and automake hardcoded to work on HDF
# machines.  Thus is enforces that HDF5 always uses the
# same versions of autotools.
# Uses automake version 1.6.3
# Uses autoconf version 2.59
# Includes macros from libtool version 1.4.2

# The autotools live in AFS, so as long as their paths don't change
# and this machine has the right version of m4, this script should
# be able to run the autotools.


  # Run commands in order
  echo /afs/ncsa/projects/hdf/packages/automake_1.6.3/bin/aclocal -I /afs/ncsa/projects/hdf/packages/libtool-1.4.2/Linux_2.4/share/aclocal
  /afs/ncsa/projects/hdf/packages/automake_1.6.3/bin/aclocal -I /afs/ncsa/projects/hdf/packages/libtool-1.4.2/Linux_2.4/share/aclocal

  echo /afs/ncsa/projects/hdf/packages/autoconf_2.59/bin/autoheader
  /afs/ncsa/projects/hdf/packages/autoconf_2.59/bin/autoheader

  echo /afs/ncsa/projects/hdf/packages/automake_1.6.3/bin/automake --foreign
  /afs/ncsa/projects/hdf/packages/automake_1.6.3/bin/automake --foreign

  echo /afs/ncsa/projects/hdf/packages/autoconf_2.59/bin/autoconf
  /afs/ncsa/projects/hdf/packages/autoconf_2.59/bin/autoconf


exit 0
