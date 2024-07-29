#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

# This file is for use of h5cc created with the CMake process
# HDF5_HOME is expected to be set

srcdir=..
builddir=.
verbose=yes
nerrors=0

# HDF5 compile commands, assuming they are in your $PATH.
H5FC=$HDF5_HOME/bin/h5fc
LD_LIBRARY_PATH=$HDF5_HOME/lib
export LD_LIBRARY_PATH

if ! test -f $H5FC; then
    echo "Set paths for H5FC and LD_LIBRARY_PATH in test.sh"
    echo "Set environment variable HDF5_HOME to the hdf5 install dir"
    echo "h5fc was not found at $H5FC"
    exit $EXIT_FAILURE
fi

H5DUMP=`echo $H5FC | sed -e 's/\/[^/]*$/\/h5dump/'`;
H5_LIBVER=$($H5FC -showconfig | grep -i "HDF5 Version:" | sed 's/^.* //g' | sed 's/[-].*//g')
H5_APIVER=$($H5FC -showconfig | grep -i "Default API mapping:" | sed 's/^.* //g' | sed 's/v//g' | sed 's/1/1_/')

H5_MAJORVER=$(echo $H5_LIBVER | cut -f1 -d'.'  | sed -E 's/\./_/g')
H5_MINORVER=$(echo $H5_LIBVER | cut -f2 -d'.'  | sed -E 's/\./_/g')
H5_RELEASEVER=$(echo $H5_LIBVER | cut -f3 -d'.'  | sed -E 's/\./_/g')
H5_LIBVER_DIR=$H5_MAJORVER$H5_MINORVER

# Shell commands used in Makefiles
RM="rm -rf"
DIFF="diff -c"
CMP="cmp -s"
GREP='grep'
CP="cp -p"  # Use -p to preserve mode,ownership,timestamps
DIRNAME='dirname'
LS='ls'
AWK='awk'

# setup plugin path
ENVCMD="env HDF5_PLUGIN_PATH=$LD_LIBRARY_PATH/plugin"

TESTDIR=$builddir


case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"


exout() {
    $*
}

dumpout() {
    $H5DUMP $*
}

return_val=0


$ECHO_N "Testing FORTRAN/H5G/h5ex_g_create...$ECHO_C"
./h5ex_g_create
dumpout h5ex_g_create.h5 >tmp.test
rm -f h5ex_g_create.h5
cmp -s tmp.test $srcdir/tfiles/18/h5ex_g_create.ddl
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`

$H5FC $srcdir/h5ex_g_compact.F90 -o h5ex_g_compact

$ECHO_N "Testing FORTRAN/H5G/h5ex_g_compact...$ECHO_C"
./h5ex_g_compact >/dev/null
dumpout h5ex_g_compact1.h5 >tmp.test
cmp -s tmp.test $srcdir/tfiles/18/h5ex_g_compact1.ddl
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
  dumpout h5ex_g_compact2.h5 >tmp.test
  cmp -s tmp.test $srcdir/tfiles/18/h5ex_g_compact2.ddl
  status=$?
  if test $status -ne 0
  then
      echo "  FAILED!"
  else
      echo "  Passed"
  fi
fi
return_val=`expr $status + $return_val`
rm -f h5ex_g_compact1.h5
rm -f h5ex_g_compact2.h5

$H5FC $srcdir/h5ex_g_phase.F90 -o h5ex_g_phase

$ECHO_N "Testing FORTRAN/H5G/h5ex_g_phase...$ECHO_C"
exout ./h5ex_g_phase >tmp.test
cmp -s tmp.test $srcdir/tfiles/18/h5ex_g_phase.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`
rm -f h5ex_g_phase.h5

$H5FC $srcdir/h5ex_g_corder.F90 -o h5ex_g_corder

$ECHO_N "Testing FORTRAN/H5G/h5ex_g_corder...$ECHO_C"
exout ./h5ex_g_corder >tmp.test
cmp -s tmp.test $srcdir/tfiles/18/h5ex_g_corder.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`
rm -f h5ex_g_corder.h5


rm -f tmp.test
echo "$return_val tests failed in /FORTRAN/H5G/"
exit $return_val
