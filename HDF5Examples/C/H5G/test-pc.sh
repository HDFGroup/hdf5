#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

# This file is for use of h5cc created with the CMake process.
# Environment variable, HDF5_HOME is expected to be set.
# $1 is the path name of the source directory.
# $2 is the path name of the build directory.
# $3 is the current path name.

top_srcdir=$1
top_builddir=$2
currentpath=$3
verbose=yes
nerrors=0

echo "Current build directory: $top_builddir/$currentpath"

# HDF5 compile commands, assuming they are in your $PATH.
H5CC=$HDF5_HOME/bin/h5cc
LD_LIBRARY_PATH=$HDF5_HOME/lib
export LD_LIBRARY_PATH

if ! test -f $H5CC; then
    echo "Set paths for H5CC and LD_LIBRARY_PATH in test.sh"
    echo "Set environment variable HDF5_HOME to the hdf5 install dir"
    echo "h5cc was not found at $H5CC"
    exit $EXIT_FAILURE
fi

H5DUMP=`echo $H5CC | sed -e 's/\/[^/]*$/\/h5dump/'`;
H5_LIBVER=$($H5CC -showconfig | grep -i "HDF5 Version:" | sed 's/^.* //g' | sed 's/[-].*//g')
H5_APIVER=$($H5CC -showconfig | grep -i "Default API mapping:" | sed 's/^.* //g' | sed 's/v//g' | sed 's/1/1_/')

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

TESTDIR=$top_builddir/$currentpath


case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"


exout() {
    cd $TESTDIR
    "$@"
}

dumpout() {
    cd $TESTDIR
    $H5DUMP "$@"
}

compileout() {
    cd $TESTDIR
    $H5CC "$@"
}

return_val=0

compileout $top_srcdir/$currentpath/h5ex_g_create.c -o h5ex_g_create

$ECHO_N "Testing C/H5G/h5ex_g_create...$ECHO_C"
exout ./h5ex_g_create
dumpout h5ex_g_create.h5 >tmp.test
rm -f $TESTDIR/h5ex_g_create.h5
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/h5ex_g_create.ddl
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`

compileout $top_srcdir/$currentpath/h5ex_g_iterate.c -o h5ex_g_iterate

$ECHO_N "Testing C/H5G/h5ex_g_iterate...$ECHO_C"
if test -f $TESTDIR/h5ex_g_iterate.h5
then
    exout ./h5ex_g_iterate >tmp.test
else
    cp $top_srcdir/$currentpath/h5ex_g_iterate.h5 $TESTDIR/h5ex_g_iterate.h5
    exout ./h5ex_g_iterate >tmp.test
    rm  -f $TESTDIR/h5ex_g_iterate.h5
fi
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/h5ex_g_iterate.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`

compileout $top_srcdir/$currentpath/h5ex_g_traverse.c -o h5ex_g_traverse

$ECHO_N "Testing C/H5G/h5ex_g_traverse...$ECHO_C"
if test -f $TESTDIR/h5ex_g_traverse.h5
then
    exout ./h5ex_g_traverse >tmp.test
else
    cp $top_srcdir/$currentpath/h5ex_g_traverse.h5 $TESTDIR/h5ex_g_traverse.h5
    exout ./h5ex_g_traverse >tmp.test
    rm  -f $TESTDIR/h5ex_g_traverse.h5
fi
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/h5ex_g_traverse.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`

compileout $top_srcdir/$currentpath/h5ex_g_visit.c -o h5ex_g_visit

$ECHO_N "Testing C/H5G/h5ex_g_visit...$ECHO_C"
if test -f $TESTDIR/h5ex_g_visit.h5
then
    exout ./h5ex_g_visit >tmp.test
else
    cp $top_srcdir/$currentpath/h5ex_g_visit.h5 $TESTDIR/h5ex_g_visit.h5
    exout ./h5ex_g_visit >tmp.test
    rm  -f $TESTDIR/h5ex_g_visit.h5
fi
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_visit.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`

compileout $top_srcdir/$currentpath/h5ex_g_compact.c -o h5ex_g_compact

$ECHO_N "Testing C/H5G/h5ex_g_compact...$ECHO_C"
exout ./h5ex_g_compact >tmp.test
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_compact.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
  dumpout h5ex_g_compact1.h5 >tmp.test
  cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_compact1.ddl
  status=$?
  if test $status -ne 0
  then
      echo "  FAILED!"
  else
    dumpout h5ex_g_compact2.h5 >tmp.test
    cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_compact2.ddl
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        echo "  Passed"
    fi
  fi
fi
return_val=`expr $status + $return_val`
rm -f $TESTDIR/h5ex_g_compact1.h5
rm -f $TESTDIR/h5ex_g_compact2.h5

compileout $top_srcdir/$currentpath/h5ex_g_phase.c -o h5ex_g_phase

$ECHO_N "Testing C/H5G/h5ex_g_phase...$ECHO_C"
exout ./h5ex_g_phase >tmp.test
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_phase.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`
rm -f $TESTDIR/h5ex_g_phase.h5

compileout $top_srcdir/$currentpath/h5ex_g_corder.c -o h5ex_g_corder

$ECHO_N "Testing C/H5G/h5ex_g_corder...$ECHO_C"
exout ./h5ex_g_corder >tmp.test
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_corder.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`
rm -f $TESTDIR/h5ex_g_corder.h5

compileout $top_srcdir/$currentpath/h5ex_g_intermediate.c -o h5ex_g_intermediate

$ECHO_N "Testing C/H5G/h5ex_g_intermediate...$ECHO_C"
exout ./h5ex_g_intermediate >tmp.test
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/h5ex_g_intermediate.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`
rm -f $TESTDIR/h5ex_g_intermediate.h5


rm -f $TESTDIR/tmp.test
echo "$return_val tests failed in C/H5G/"
exit $return_val
