#! /bin/sh
# A simple script to reconfigure autotools for HDF5.
# The autotools include a reconfigure script, but this script
# has the paths of autoconf and automake hardcoded to work on HDF
# machines.  Thus is enforces that HDF5 always uses the
# same versions of autotools.
# Uses automake version 1.6.3
# Uses autoconf version 2.59

# Discover which machine this script is being run on.
# Right now, heping is the only machine with the correct
# versions of autoconf and automake installed.

HOSTNAME=$(hostname)

if [ "$HOSTNAME" != "heping" ]; then
  echo "HDF5 can only be reconfigured on heping, sorry."
else

  # If this is heping, run commands in order
  echo /usr/bin/aclocal
  /usr/bin/aclocal
  echo /usr/local/autoconf-2.59/bin/autoheader
  /usr/local/autoconf-2.59/bin/autoheader
  echo /usr/bin/automake --foreign
  /usr/bin/automake --foreign
  echo /usr/local/autoconf-2.59/bin/autoconf
  /usr/local/autoconf-2.59/bin/autoconf

fi

exit 0
